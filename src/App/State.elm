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
import Queue.State as Queue
import Routing.State as Routing
import Settings.State as Settings
import Sources.State as Sources
import Tracks.State as Tracks


-- Children, Pt. 2

import Queue.Ports
import Queue.Types
import Queue.Utils
import Routing.Types as RT
import Sources.ContextMenu
import Sources.Types
import Tracks.ContextMenu
import Tracks.Encoding
import Tracks.Ports
import Tracks.Types
import Tracks.Utils


-- 💧


initialModel : RT.Page -> Model
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
    , queue = Queue.initialModel
    , routing = Routing.initialModel initialPage
    , settings = Settings.initialModel
    , sources = Sources.initialModel
    , tracks = Tracks.initialModel
    }


initialCommands : RT.Page -> Cmd Msg
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

                encodedTracks =
                    List.map Tracks.Encoding.encodeTrack newModel.tracks.collection.untouched
            in
                (!)
                    newModel
                    [ Tracks.Ports.updateSearchIndex encodedTracks
                    , Queue.Ports.toggleRepeat newModel.queue.repeat
                    , Equalizer.adjustAllKnobs newModel.equalizer

                    -- Tracks
                    , ( newModel.tracks
                      , newModel.tracks.collection
                      )
                        |> Tracks.Types.InitialCollection
                        |> TracksMsg
                        |> doDelayed (Time.millisecond * 250)

                    --
                    , doDelayed (Time.millisecond * 500) FillQueue
                    , doDelayed (Time.millisecond * 500) HideLoadingScreen
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
                    |> RT.ErrorScreen
                    |> RT.SetPage
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
                { model | storageDebounce = debounce } ! [ cmd ]

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

        RoutingMsg sub ->
            Routing.update sub model.routing
                |> mapModel (\x -> { model | routing = x })
                |> handleRouteTransitions sub model

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

        CleanQueue ->
            (!)
                model
                [ model.tracks.collection.harvested
                    |> List.map Tracks.Utils.unindentify
                    |> Queue.Types.Clean
                    |> QueueMsg
                    |> do
                ]

        FillQueue ->
            (!)
                model
                [ model.tracks.collection.harvested
                    |> List.map Tracks.Utils.unindentify
                    |> Queue.Types.Fill model.timestamp
                    |> QueueMsg
                    |> do
                ]

        RecalibrateTracks ->
            (!)
                model
                [ Tracks.Types.Recalibrate
                    |> TracksMsg
                    |> do
                ]

        ResetQueue ->
            (!)
                model
                [ Queue.Types.Reset
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
        -- Children, Pt. 3
        ------------------------------------
        ShowSourceMenu sourceId mousePos ->
            (!)
                { model | contextMenu = Just (Sources.ContextMenu.listMenu sourceId mousePos) }
                []

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
                (!)
                    { model | contextMenu = contextMenu }
                    []

        ShowViewMenu ->
            (!)
                { model | contextMenu = Nothing }
                [ case model.contextMenu of
                    Just _ ->
                        Cmd.none

                    Nothing ->
                        Task.perform ShowViewMenuWithWindow Window.size
                ]

        ShowViewMenuWithWindow windowSize ->
            { x = windowSize.width // 2
            , y = windowSize.height // 2
            }
                |> Tracks.ContextMenu.viewMenu
                |> Just
                |> (\c -> { model | contextMenu = c })
                |> (\m -> ( m, Cmd.none ))

        ------------------------------------
        -- Other
        ------------------------------------
        NoOp ->
            (!) model []



-- 🔥 / Debounce configurations


debounceStoreUserDataConfig : Debounce.Config Msg
debounceStoreUserDataConfig =
    { strategy = Debounce.later (5 * Time.second)
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


handleRouteTransitions : RT.Msg -> Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleRouteTransitions routingMsg oldModel response =
    response
        ------------------------------------
        -- Commands
        ------------------------------------
        |> Tuple.mapSecond
            (\cmd ->
                case oldModel.routing.currentPage of
                    RT.Index ->
                        Cmd.batch [ cmd, do RecalibrateTracks ]

                    _ ->
                        cmd
            )
        ------------------------------------
        -- Model
        ------------------------------------
        |> Tuple.mapFirst
            (\model ->
                case routingMsg of
                    --
                    -- When we are going to edit a source,
                    -- set the `form` attribute.
                    --
                    RT.SetPage (RT.Sources (Sources.Types.Edit sourceId)) ->
                        let
                            sources =
                                Sources.editForm model.sources sourceId
                        in
                            { model | sources = sources }

                    --
                    -- When we are going to create a source,
                    -- set the `form` attribute.
                    --
                    RT.SetPage (RT.Sources Sources.Types.New) ->
                        case model.sources.form of
                            Sources.Types.EditForm _ ->
                                { model | sources = Sources.newForm model.sources }

                            _ ->
                                model

                    --
                    -- Default
                    --
                    _ ->
                        model
            )
