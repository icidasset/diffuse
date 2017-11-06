module State exposing (..)

import Date
import Debounce
import Dict
import Dict.Ext as Dict
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Navigation
import Ports
import Response exposing (..)
import Response.Ext as Response exposing (do, doDelayed)
import Slave.Translations
import Slave.Types exposing (AlienMsg(..))
import Task
import Time
import Types exposing (..)
import Utils exposing (displayError, displayMessage)
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

import Authentication.Events
import Authentication.Types
import Authentication.UserData
import Playlists.Types
import Playlists.Utils
import Queue.Ports
import Queue.Types
import Queue.Utils
import Routing.Transitions
import Routing.Types exposing (Page(..))
import Sources.ContextMenu
import Sources.Encoding
import Sources.Types
import Tracks.ContextMenu
import Tracks.Encoding
import Tracks.Ports
import Tracks.Types
import Tracks.Utils


-- 💧


initialModel : ProgramFlags -> Page -> Model
initialModel flags initialPage =
    { contextMenu = Nothing
    , isHTTPS = flags.isHTTPS
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


initialCommand : ProgramFlags -> Page -> Cmd Msg
initialCommand _ initialPage =
    Cmd.batch
        [ -- Time
          Task.perform SetTimestamp Time.now

        -- Children
        , Authentication.initialCommand
        , Routing.initialCommand initialPage
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
            let
                flags =
                    { isHTTPS = model.isHTTPS }
            in
                (!)
                    (initialModel flags model.routing.currentPage)
                    [ initialCommand flags model.routing.currentPage ]

        SetIsTouchDevice bool ->
            (!) { model | isTouchDevice = bool } []

        ------------------------------------
        -- Data in
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

                    --
                    , case newModel.tracks.searchTerm of
                        Just _ ->
                            Cmd.none

                        Nothing ->
                            doDelayed (Time.millisecond * 250) HideLoadingScreen
                    ]

        ------------------------------------
        -- Data out
        ------------------------------------
        StoreUserData ->
            (!)
                model
                [ Authentication.Events.issueWithData
                    Authentication.Types.StoreData
                    (model
                        |> Authentication.UserData.outwards
                        |> Encode.string
                    )
                ]

        ------------------------------------
        -- Data out / Debounced
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

        ------------------------------------
        -- Time
        ------------------------------------
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
            model.abroad
                |> Abroad.update sub
                |> mapModel (\x -> { model | abroad = x })

        AuthenticationMsg sub ->
            model.authentication
                |> Authentication.update sub
                |> mapModel (\x -> { model | authentication = x })

        ConsoleMsg sub ->
            model.console
                |> Console.update sub
                |> mapModel (\x -> { model | console = x })

        EqualizerMsg sub ->
            model.equalizer
                |> Equalizer.update sub
                |> mapModel (\x -> { model | equalizer = x })

        QueueMsg sub ->
            model.queue
                |> Queue.update sub
                |> mapModel (\x -> { model | queue = x })

        PlaylistsMsg sub ->
            model.playlists
                |> Playlists.update sub
                |> mapModel (\x -> { model | playlists = x })

        RoutingMsg sub ->
            model.routing
                |> Routing.update sub
                |> mapModel (\x -> { model | routing = x })
                |> Routing.Transitions.transition sub model

        SettingsMsg sub ->
            model.settings
                |> Settings.update sub
                |> mapModel (\x -> { model | settings = x })

        SourcesMsg sub ->
            model.sources
                |> Sources.update sub
                |> mapModel (\x -> { model | sources = x })

        TracksMsg sub ->
            model.tracks
                |> Tracks.update sub
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

        Types.ProcessSources ->
            let
                tracks =
                    List.map
                        Tracks.Encoding.encodeTrack
                        model.tracks.collection.untouched

                sources =
                    List.map
                        Sources.Encoding.encode
                        model.sources.collection

                data =
                    Encode.object
                        [ ( "tracks", Encode.list tracks )
                        , ( "sources", Encode.list sources )
                        ]

                sourcesModel =
                    model.sources

                isProcessing =
                    Just model.sources.collection
            in
                (!)
                    { model
                        | sources =
                            { sourcesModel
                                | isProcessing = isProcessing
                                , processingErrors = []
                            }
                    }
                    [ Ports.slaveEvent
                        { tag = "PROCESS_SOURCES"
                        , data = data
                        , error = Nothing
                        }
                    ]

        ------------------------------------
        -- Slave events
        ------------------------------------
        --
        -- Sources
        --
        Extraterrestrial ProcessSourcesCompleted (Ok _) ->
            model.sources
                |> (\s -> { model | sources = { s | isProcessing = Nothing } })
                |> (\m -> ( m, Cmd.none ))

        Extraterrestrial ReportProcessingError (Ok result) ->
            let
                err =
                    Result.withDefault
                        Dict.empty
                        (decodeValue (dict string) result)

                errors =
                    (::)
                        ( Dict.fetch "sourceId" "BEEP" err, Dict.fetch "message" "BOOP" err )
                        model.sources.processingErrors
            in
                model.sources
                    |> (\s -> { model | sources = { s | processingErrors = errors } })
                    |> (\m -> ( m, Cmd.none ))

        --
        -- Tracks
        --
        Extraterrestrial AddTracks (Ok result) ->
            (!)
                model
                [ case decodeValue (Decode.list Tracks.Encoding.trackDecoder) result of
                    Ok tracks ->
                        tracks
                            |> Tracks.Types.Add
                            |> TracksMsg
                            |> do

                    Err err ->
                        displayError err
                ]

        Extraterrestrial RemoveTracksByPath (Ok result) ->
            (!)
                model
                [ case decodeValue (dict value) result of
                    Ok dictionary ->
                        let
                            filePaths =
                                dictionary
                                    |> Dict.fetch "filePaths" (Encode.list [])
                                    |> Decode.decodeValue (list string)
                                    |> Result.withDefault []

                            nonExistantSid =
                                "BEEP_BOOP"

                            sourceId =
                                dictionary
                                    |> Dict.fetch "sourceId" (Encode.string nonExistantSid)
                                    |> Decode.decodeValue string
                                    |> Result.withDefault nonExistantSid
                        in
                            filePaths
                                |> Tracks.Types.RemoveByPath sourceId
                                |> TracksMsg
                                |> do

                    Err err ->
                        displayError err
                ]

        --
        -- Other things
        --
        Extraterrestrial ReportError (Err err) ->
            ( model, displayError err )

        --
        -- Ignore other messages
        --
        Extraterrestrial _ _ ->
            ( model, Cmd.none )

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
            ( model, Cmd.none )



-- 🔥 / Debounce configurations


debounceStoreUserDataConfig : Debounce.Config Msg
debounceStoreUserDataConfig =
    { strategy = Debounce.later (2.5 * Time.second)
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
        , Ports.slaveEventResult handleSlaveResult

        -- Children
        , Sub.map AbroadMsg <| Abroad.subscriptions model.abroad
        , Sub.map AuthenticationMsg <| Authentication.subscriptions model.authentication
        , Sub.map ConsoleMsg <| Console.subscriptions model.console
        , Sub.map EqualizerMsg <| Equalizer.subscriptions model.equalizer
        , Sub.map QueueMsg <| Queue.subscriptions model.queue
        , Sub.map SourcesMsg <| Sources.subscriptions model.sources
        , Sub.map TracksMsg <| Tracks.subscriptions model.tracks
        ]


handleSlaveResult : AlienEvent -> Msg
handleSlaveResult event =
    Extraterrestrial
        (Slave.Translations.stringToAlienMessage event.tag)
        (case event.error of
            Just err ->
                Err err

            Nothing ->
                Ok event.data
        )
