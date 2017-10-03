module State exposing (..)

import Authentication.Method
import Authentication.UserData
import Date
import Debounce
import List.Extra as List
import Maybe.Extra as Maybe
import Navigation
import Ports
import Response exposing (..)
import Response.Ext as Response exposing (do, doDelayed)
import Task
import Time
import Types exposing (..)
import Window


-- Children

import Abroad.State as Abroad
import Authentication.State as Authentication
import Console.State as Console
import Equalizer.State as Equalizer
import Playlists.State as Playlists
import Queue.State as Queue
import Routing.State as Routing
import Settings.State as Settings
import Sources.State as Sources
import Tracks.State as Tracks


-- Children, Pt. 2

import Playlists.Types
import Playlists.Utils
import Queue.Ports
import Queue.Types
import Queue.Utils
import Routing.Transitions
import Routing.Types exposing (Page(..))
import Sources.ContextMenu
import Sources.Types
import Tracks.ContextMenu
import Tracks.Encoding
import Tracks.Ports
import Tracks.Types
import Tracks.Utils


-- 💧


initialModel : Page -> Model
initialModel initialPage =
    { contextMenu = Nothing
    , isTouchDevice = False
    , showLoadingScreen = True

    ------------------------------------
    -- Time
    ------------------------------------
    , storageDebounce = Debounce.init
    , timestamp = Date.fromTime 0

    ------------------------------------
    -- Children
    ------------------------------------
    , abroad = Abroad.initialModel
    , authentication = Authentication.initialModel
    , console = Console.initialModel
    , equalizer = Equalizer.initialModel
    , playlists = Playlists.initialModel
    , queue = Queue.initialModel
    , routing = Routing.initialModel initialPage
    , settings = Settings.initialModel
    , sources = Sources.initialModel
    , tracks = Tracks.initialModel
    }


initialCommands : Page -> Cmd Msg
initialCommands initialPage =
    Cmd.batch
        [ -- Time
          Task.perform SetTimestamp Time.now

        -- Children
        , Authentication.initialCommands
        , Routing.initialCommands initialPage
        ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickAway ->
            (!) { model | contextMenu = Nothing } []

        HideLoadingScreen ->
            (!) { model | showLoadingScreen = False } []

        ShowLoadingScreen ->
            (!) { model | showLoadingScreen = True } []

        Reset ->
            (!)
                (initialModel model.routing.currentPage)
                [ initialCommands model.routing.currentPage ]

        SetIsTouchDevice bool ->
            (!) { model | isTouchDevice = bool } []

        ------------------------------------
        -- User layer
        ------------------------------------
        ImportUserData json ->
            let
                newModel =
                    Authentication.UserData.inwards json model

                tracks =
                    newModel.tracks
            in
                (!)
                    newModel
                    [ Queue.Ports.toggleRepeat newModel.queue.repeat
                    , Equalizer.adjustAllKnobs newModel.equalizer

                    --
                    , ( { tracks | collection = Tracks.Types.emptyCollection }
                      , tracks.collection
                      )
                        |> Tracks.Types.InitialCollection
                        |> TracksMsg
                        |> do
                    ]

        StoreUserData ->
            (!)
                model
                [ case model.authentication.method of
                    Just method ->
                        model
                            |> Authentication.UserData.outwards
                            |> Authentication.Method.storeData method
                            |> Task.attempt DidStoreUserData

                    Nothing ->
                        Cmd.none
                ]

        DidStoreUserData (Ok _) ->
            -- Carry on
            (!) model []

        DidStoreUserData (Err err) ->
            (!)
                model
                [ err
                    |> String.append "User data storage error: "
                    |> Routing.Types.ErrorScreen
                    |> Routing.Types.SetPage
                    |> RoutingMsg
                    |> do
                ]

        ------------------------------------
        -- Time
        ------------------------------------
        DebounceStoreUserData ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceStoreUserDataConfig () model.storageDebounce
            in
                (!)
                    { model | storageDebounce = debounce }
                    [ cmd ]

        DebounceCallbackStoreUserData msg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceStoreUserDataConfig
                        (Debounce.takeLast (always <| do StoreUserData))
                        msg
                        model.storageDebounce
            in
                (!)
                    { model | storageDebounce = debounce }
                    [ cmd ]

        SetTimestamp time ->
            let
                stamp =
                    Date.fromTime time

                sources =
                    model.sources
            in
                (!)
                    { model
                        | sources = { sources | timestamp = stamp }
                        , timestamp = stamp
                    }
                    []

        ------------------------------------
        -- Children
        ------------------------------------
        AbroadMsg sub ->
            Abroad.update sub model.abroad
                |> mapModel (\x -> { model | abroad = x })

        AuthenticationMsg sub ->
            Authentication.update sub model.authentication
                |> mapModel (\x -> { model | authentication = x })

        ConsoleMsg sub ->
            Console.update sub model.console
                |> mapModel (\x -> { model | console = x })

        EqualizerMsg sub ->
            Equalizer.update sub model.equalizer
                |> mapModel (\x -> { model | equalizer = x })

        QueueMsg sub ->
            Queue.update sub model.queue
                |> mapModel (\x -> { model | queue = x })

        PlaylistsMsg sub ->
            Playlists.update sub model.playlists
                |> mapModel (\x -> { model | playlists = x })

        RoutingMsg sub ->
            Routing.update sub model.routing
                |> mapModel (\x -> { model | routing = x })
                |> Routing.Transitions.transition sub model

        SettingsMsg sub ->
            Settings.update sub model.settings
                |> mapModel (\x -> { model | settings = x })

        SourcesMsg sub ->
            Sources.update sub model.sources
                |> mapModel (\x -> { model | sources = x })

        TracksMsg sub ->
            Tracks.update sub model.tracks
                |> mapModel (\x -> { model | tracks = x })

        ------------------------------------
        -- Children, Pt. 2
        ------------------------------------
        ActiveQueueItemChanged maybeQueueItem ->
            (!)
                model
                [ -- `activeQueueItemChanged` port
                  maybeQueueItem
                    |> Maybe.map
                        .track
                    |> Maybe.map
                        (Queue.Utils.makeEngineItem
                            model.timestamp
                            model.sources.collection
                        )
                    |> Queue.Ports.activeQueueItemChanged

                -- Identify
                , maybeQueueItem
                    |> Maybe.map .track
                    |> Tracks.Types.SetActiveTrackId
                    |> TracksMsg
                    |> do
                ]

        AutoGeneratePlaylists ->
            model.playlists
                |> Playlists.update
                    (model.tracks.collection.untouched
                        |> Playlists.Utils.autoGenerate model.sources.collection
                        |> Playlists.Types.SetCollection
                    )
                |> mapModel (\x -> { model | playlists = x })

        FillQueue ->
            (!)
                model
                [ model.tracks.collection.harvested
                    |> List.map Tracks.Utils.unindentify
                    |> Queue.Types.Fill model.timestamp
                    |> QueueMsg
                    |> do
                ]

        PlayTrack index ->
            (!)
                model
                [ index
                    |> String.toInt
                    |> Result.toMaybe
                    |> Maybe.andThen (\idx -> List.getAt idx model.tracks.collection.exposed)
                    |> Maybe.map Tracks.Utils.unindentify
                    |> Maybe.map Queue.Types.InjectFirstAndPlay
                    |> Maybe.map QueueMsg
                    |> Maybe.map do
                    |> Maybe.withDefault Cmd.none
                ]

        ProcessSources ->
            (!)
                model
                [ model.tracks.collection.untouched
                    |> Sources.Types.Process
                    |> SourcesMsg
                    |> do
                ]

        ToggleFavourite index ->
            (!)
                model
                [ index
                    |> Tracks.Types.ToggleFavourite
                    |> TracksMsg
                    |> do
                ]

        ------------------------------------
        -- Context Menu
        ------------------------------------
        ShowSourceMenu sourceId mousePos ->
            let
                contextMenu =
                    mousePos
                        |> Sources.ContextMenu.listMenu sourceId
                        |> Just
            in
                (!) { model | contextMenu = contextMenu } []

        ShowTrackContextMenu ( index, mousePos ) ->
            let
                contextMenu =
                    index
                        |> String.toInt
                        |> Result.toMaybe
                        |> Maybe.andThen (\idx -> List.getAt idx model.tracks.collection.exposed)
                        |> Maybe.map Tracks.ContextMenu.trackMenu
                        |> Maybe.map (\fn -> fn mousePos)
            in
                (!) { model | contextMenu = contextMenu } []

        ------------------------------------
        -- Other
        ------------------------------------
        NoOp ->
            (!) model []



-- 🔥 / Debounce configurations


debounceStoreUserDataConfig : Debounce.Config Msg
debounceStoreUserDataConfig =
    { strategy = Debounce.later (3 * Time.second)
    , transform = DebounceCallbackStoreUserData
    }



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Time
          Time.every (1 * Time.minute) SetTimestamp

        -- Ports
        , Ports.setIsTouchDevice SetIsTouchDevice

        -- Children
        , Sub.map ConsoleMsg <| Console.subscriptions model.console
        , Sub.map EqualizerMsg <| Equalizer.subscriptions model.equalizer
        , Sub.map QueueMsg <| Queue.subscriptions model.queue
        , Sub.map SourcesMsg <| Sources.subscriptions model.sources
        , Sub.map TracksMsg <| Tracks.subscriptions model.tracks
        ]
