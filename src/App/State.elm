module State exposing (..)

import Date
import Debounce
import Extraterrestrial
import Json.Encode as Encode
import Keyboard.Extra as Keyboard
import List.Extra as List
import Maybe.Extra as Maybe
import Notifications
import Notifications.Config
import Notifications.Types
import Ports
import Response exposing (..)
import Response.Ext as Response exposing (do, doDelayed)
import Slave.Translations
import Slave.Types exposing (AlienMsg(..))
import Task
import Time
import Toasty
import Types exposing (..)
import Utils exposing (displayError)


-- Children

import Abroad.State as Abroad
import Alfred.State as Alfred
import Authentication.State as Authentication
import Console.State as Console
import ContextMenu.State as ContextMenu
import Equalizer.State as Equalizer
import Playlists.State as Playlists
import Queue.State as Queue
import Routing.State as Routing
import Settings.State as Settings
import Sources.State as Sources
import Tracks.State as Tracks


-- Children, Pt. 2

import Alfred.Types
import Authentication.Events
import Authentication.Types
import Authentication.UserData
import Console.Types
import Playlists.Alfred
import Playlists.AutoGenerate
import Playlists.Types
import Queue.Ports
import Queue.Types
import Queue.Utils
import Routing.Transitions
import Routing.Types exposing (Page(..))
import Sources.Encoding
import Sources.Types
import Sources.Utils exposing (pickEnableSourceIds, viableSourcesOnly)
import Tracks.ContextMenu
import Tracks.Encoding
import Tracks.Types
import Tracks.View


-- 💧


initialModel : ProgramFlags -> Page -> String -> Model
initialModel flags initialPage origin =
    { holdingShiftKey = False
    , isDevelopmentEnvironment = flags.isDevelopmentEnvironment
    , isElectron = flags.isElectron
    , isHTTPS = flags.isHTTPS
    , isTouchDevice = False
    , origin = origin
    , screenHeight = flags.screenHeight
    , showLoadingScreen = True
    , toasties = Toasty.initialState

    ------------------------------------
    -- Time
    ------------------------------------
    , ludStorageDebounce = Debounce.init
    , udStorageDebounce = Debounce.init
    , timestamp = Date.fromTime 0

    ------------------------------------
    -- Children
    ------------------------------------
    , abroad = Abroad.initialModel
    , alfred = Alfred.initialModel
    , authentication = Authentication.initialModel
    , console = Console.initialModel
    , contextMenu = ContextMenu.initialModel
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


{-| Main `update` function, all state goes through here.

{!} Some messages here should ideally be in their child `update` function,
but as they involve multiple other state pieces (ie. other children)
they are located here. These will be marked with `{🍭}`, which symbolizes
complexity, or in other words, intertwined with other state.

-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- For when you click somewhere on the screen,
        -- and something needs to disappear.
        ClickAway ->
            let
                a =
                    model.alfred

                alfred =
                    { a | instance = Nothing }

                c =
                    model.contextMenu

                contextMenu =
                    { c | instance = Nothing }
            in
                (!) { model | alfred = alfred, contextMenu = contextMenu } []

        -- Sometimes you just wanna chill.
        NoOp ->
            (!) model []

        -- Reset the entire application state.
        -- This usually happens when you sign out.
        Reset ->
            let
                flags =
                    { isDevelopmentEnvironment = model.isDevelopmentEnvironment
                    , isElectron = model.isElectron
                    , isHTTPS = model.isHTTPS
                    , screenHeight = model.screenHeight
                    }

                initModel =
                    initialModel flags model.routing.currentPage model.origin

                newModel =
                    initModel.settings
                        |> (\s -> { s | fadeInLastBackdrop = False })
                        |> (\s -> { s | loadedBackdrops = model.settings.loadedBackdrops })
                        |> (\s -> { initModel | settings = s })
            in
                (!)
                    newModel
                    [ initialCommand flags model.routing.currentPage ]

        -- We have a message for this because this value comes
        -- in through a port. The reason for this is that the
        -- detection happens on the first "touchstart" event.
        SetIsTouchDevice bool ->
            (!) { model | isTouchDevice = bool } []

        ------------------------------------
        -- Keyboard (Down)
        ------------------------------------
        KeydownMsg Keyboard.Escape ->
            (!) model [ do ClickAway ]

        KeydownMsg Keyboard.Shift ->
            (!) { model | holdingShiftKey = True } []

        KeydownMsg _ ->
            (,) model Cmd.none

        ------------------------------------
        -- Keyboard (Up)
        ------------------------------------
        KeyupMsg Keyboard.Shift ->
            (!) { model | holdingShiftKey = False } []

        KeyupMsg _ ->
            (,) model Cmd.none

        ------------------------------------
        -- Libraries
        ------------------------------------
        ToastyMsg sub ->
            Toasty.update Notifications.Config.config ToastyMsg sub model

        ------------------------------------
        -- Loading
        ------------------------------------
        HideLoadingScreen ->
            (!) { model | showLoadingScreen = False } []

        ShowLoadingScreen ->
            (!) { model | showLoadingScreen = True } []

        ------------------------------------
        -- Notifications
        ------------------------------------
        ShowNotification notification ->
            Notifications.add notification ( model, Cmd.none )

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
        -- User layer
        ------------------------------------
        --
        -- Data in, primary data.
        --
        ImportUserData json ->
            let
                newModel =
                    Authentication.UserData.inwards json model

                tracks =
                    newModel.tracks

                processSources =
                    not model.isDevelopmentEnvironment
            in
                (!)
                    newModel
                    [ ( { tracks | collection = Tracks.Types.emptyCollection }
                      , tracks.collection
                      )
                        |> Tracks.Types.InitialCollection processSources
                        |> TracksMsg
                        |> do

                    --
                    , case newModel.tracks.searchTerm of
                        Just _ ->
                            Cmd.none

                        Nothing ->
                            doDelayed (Time.millisecond * 250) HideLoadingScreen
                    ]

        --
        -- Data in, secondary data.
        --
        ImportLocalUserData json ->
            let
                newModel =
                    Authentication.UserData.inwardsAdditional json model
            in
                (!)
                    newModel
                    [ Queue.Ports.toggleRepeat newModel.queue.repeat
                    , Equalizer.adjustAllKnobs newModel.equalizer
                    ]

        --
        -- Data in, demo data.
        --
        InsertDemoContent json ->
            let
                demoModel =
                    Authentication.UserData.inwards json model

                ( demoTracks, demoTracksCol ) =
                    ( demoModel.tracks
                    , demoModel.tracks.collection
                    )

                demoTracksEmpty =
                    { demoTracks | collection = Tracks.Types.emptyCollection }
            in
                (!)
                    { model
                        | sources = demoModel.sources
                        , tracks = demoTracks
                    }
                    [ ( demoTracksEmpty
                      , demoTracksCol
                      )
                        |> Tracks.Types.InitialCollection True
                        |> TracksMsg
                        |> do
                    ]

        --
        -- Data out, primary data.
        --
        StoreUserData ->
            if model.authentication.signedIn then
                (!)
                    model
                    [ Authentication.Events.issueWithData
                        Authentication.Types.StoreData
                        (model
                            |> Authentication.UserData.outwards
                            |> Encode.string
                        )
                    ]
            else
                (!) model []

        --
        -- Data out, secondary data.
        --
        StoreLocalUserData ->
            if model.authentication.signedIn then
                (!)
                    model
                    [ model
                        |> Authentication.UserData.outwardsAdditional
                        |> Authentication.Types.StoreLocalUserData
                        |> AuthenticationMsg
                        |> do
                    ]
            else
                (!) model []

        --
        -- Data syncing
        -- (ie. dealing with caching)
        --
        SyncCompleted result ->
            result
                |> Ok
                |> Authentication.Types.Extraterrestrial Authentication.Types.GetData
                |> AuthenticationMsg
                |> do
                |> List.singleton
                |> (!) model

        SyncStarted ->
            "⚡️ Syncing data"
                |> Notifications.Types.Message
                |> ShowNotification
                |> do
                |> List.singleton
                |> (!) model

        --
        -- Debounced messages
        --
        DebounceStoreUserData ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceStoreUserDataConfig () model.udStorageDebounce
            in
                (!)
                    { model | udStorageDebounce = debounce }
                    [ cmd ]

        DebounceCallbackStoreUserData debounceMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceStoreUserDataConfig
                        (StoreUserData
                            |> do
                            |> always
                            |> Debounce.takeLast
                        )
                        debounceMsg
                        model.udStorageDebounce
            in
                (!)
                    { model | udStorageDebounce = debounce }
                    [ cmd ]

        DebounceStoreLocalUserData ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceStoreLocalUserDataConfig () model.ludStorageDebounce
            in
                (!)
                    { model | ludStorageDebounce = debounce }
                    [ cmd ]

        DebounceCallbackStoreLocalUserData debounceMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceStoreLocalUserDataConfig
                        (StoreLocalUserData
                            |> do
                            |> always
                            |> Debounce.takeLast
                        )
                        debounceMsg
                        model.ludStorageDebounce
            in
                (!)
                    { model | ludStorageDebounce = debounce }
                    [ cmd ]

        ------------------------------------
        -- Children
        ------------------------------------
        AbroadMsg sub ->
            model.abroad
                |> Abroad.update sub
                |> mapModel (\x -> { model | abroad = x })

        AlfredMsg sub ->
            model.alfred
                |> Alfred.update sub
                |> mapModel (\x -> { model | alfred = x })

        AuthenticationMsg sub ->
            model.authentication
                |> Authentication.update sub
                |> mapModel (\x -> { model | authentication = x })

        ConsoleMsg sub ->
            model.console
                |> Console.update sub
                |> mapModel (\x -> { model | console = x })

        ContextMenuMsg sub ->
            model.contextMenu
                |> ContextMenu.update sub
                |> mapModel (\x -> { model | contextMenu = x })

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
        -- Children, Pt. 2 {🍭}
        ------------------------------------
        ApplyTrackSelection trackIndex ->
            (!)
                model
                [ trackIndex
                    |> String.toInt
                    |> Result.toMaybe
                    |> Maybe.map (Tracks.Types.ApplyTrackSelection model.holdingShiftKey)
                    |> Maybe.map (TracksMsg >> do)
                    |> Maybe.withDefault Cmd.none
                ]

        ActiveQueueItemChanged maybeQueueItem ->
            (!)
                model
                [ -- `activeQueueItemChanged` port
                  maybeQueueItem
                    |> Maybe.map
                        .identifiedTrack
                    |> Maybe.map
                        (Queue.Utils.makeEngineItem
                            model.timestamp
                            model.sources.collection
                        )
                    |> Queue.Ports.activeQueueItemChanged

                -- Identify
                , maybeQueueItem
                    |> Maybe.map .identifiedTrack
                    |> Tracks.Types.SetActiveIdentifiedTrack
                    |> TracksMsg
                    |> do
                ]

        -- Insert automatically-generated playlists.
        --
        AutoGeneratePlaylists ->
            let
                nonAutoGeneratedPlaylists =
                    List.filter
                        (.autoGenerated >> (==) False)
                        model.playlists.collection

                autoGeneratedPlaylists =
                    Playlists.AutoGenerate.autoGenerate
                        (viableSourcesOnly
                            model
                            model.sources.collection
                        )
                        model.tracks.collection.untouched

                playlists =
                    List.concat
                        [ nonAutoGeneratedPlaylists
                        , autoGeneratedPlaylists
                        ]
            in
                model.playlists
                    |> Playlists.update (Playlists.Types.SetCollection playlists)
                    |> mapModel (\x -> { model | playlists = x })

        -- Keeps the selected playlist in check.
        --
        CheckSelectedPlaylist ->
            case model.tracks.selectedPlaylist of
                Just selectedPlaylist ->
                    let
                        maybePlaylist =
                            List.find
                                (.name >> (==) selectedPlaylist.name)
                                model.playlists.collection

                        tracks =
                            model.tracks

                        commandsWhenChange =
                            [ do (TracksMsg Tracks.Types.Rearrange)
                            , do (DebounceStoreUserData)
                            ]
                    in
                        case maybePlaylist of
                            Just playlist ->
                                if playlist.tracks /= selectedPlaylist.tracks then
                                    playlist
                                        |> Just
                                        |> (\p -> { tracks | selectedPlaylist = p })
                                        |> (\t -> { model | tracks = t })
                                        |> (\m -> (!) m commandsWhenChange)
                                else
                                    (!) model []

                            Nothing ->
                                Nothing
                                    |> (\p -> { tracks | selectedPlaylist = p })
                                    |> (\t -> { model | tracks = t })
                                    |> (\m -> (!) m commandsWhenChange)

                Nothing ->
                    (!) model []

        -- Fills up the queue.
        --
        FillQueue ->
            (!)
                model
                [ model.tracks.collection.harvested
                    |> Queue.Types.Fill model.timestamp
                    |> QueueMsg
                    |> do
                ]

        -- Inserts a track into the queue and plays it.
        --
        PlayTrack index ->
            (!)
                model
                [ index
                    |> String.toInt
                    |> Result.toMaybe
                    |> Maybe.andThen (\idx -> List.getAt idx model.tracks.collection.harvested)
                    |> Maybe.map Queue.Types.InjectFirstAndPlay
                    |> Maybe.map QueueMsg
                    |> Maybe.map do
                    |> Maybe.withDefault Cmd.none
                ]

        -- Processes all sources.
        --
        Types.ProcessSources ->
            let
                tracks =
                    List.map
                        Tracks.Encoding.encodeTrack
                        model.tracks.collection.untouched

                sourcesCollection =
                    viableSourcesOnly
                        model
                        model.sources.collection

                sources =
                    List.map Sources.Encoding.encode sourcesCollection

                data =
                    Encode.object
                        [ ( "tracks", Encode.list tracks )
                        , ( "sources", Encode.list sources )
                        ]

                sourcesModel =
                    model.sources

                isProcessing =
                    Just sourcesCollection
            in
                if List.isEmpty sources then
                    (!)
                        model
                        []
                else
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

        -- Show `Alfred` for playlists.
        --
        RequestAssistanceForPlaylists tracks ->
            model.playlists.collection
                |> List.filter (.autoGenerated >> (==) False)
                |> Playlists.Alfred.create tracks
                |> Alfred.Types.Assign
                |> AlfredMsg
                |> do
                |> Response.res model

        -- Set enabled-source-ids on the tracks model.
        --
        SetEnabledSourceIds sources ->
            (!)
                model
                [ sources
                    |> viableSourcesOnly model
                    |> pickEnableSourceIds
                    |> Tracks.Types.SetEnabledSourceIds
                    |> TracksMsg
                    |> do
                ]

        -- Show a context menu for a set of tracks.
        --
        ShowTrackContextMenu ( indexStr, mousePos ) ->
            case String.toInt indexStr of
                Ok index ->
                    let
                        trackMenu =
                            Tracks.ContextMenu.trackMenu
                                model.tracks
                                model.playlists.lastModifiedPlaylist

                        ( newTracksModel, tracksCmd ) =
                            if model.isTouchDevice then
                                (,) model.tracks Cmd.none
                            else
                                Tracks.update
                                    (Tracks.Types.ApplyTrackSelectionUsingContextMenu index)
                                    (model.tracks)

                        tracks =
                            List.map
                                (\idx ->
                                    model.tracks.collection.harvested
                                        |> List.getAt idx
                                        |> Maybe.withDefault Tracks.Types.emptyIdentifiedTrack
                                )
                                (if model.isTouchDevice then
                                    [ index ]
                                 else
                                    newTracksModel.selectedTrackIndexes
                                )

                        instance =
                            Tracks.ContextMenu.trackMenu
                                model.tracks
                                model.playlists.lastModifiedPlaylist
                                tracks
                                mousePos

                        contextMenu =
                            model.contextMenu
                    in
                        (!)
                            { model
                                | contextMenu = { contextMenu | instance = Just instance }
                                , tracks = newTracksModel
                            }
                            [ tracksCmd ]

                Err _ ->
                    (!) model []

        ------------------------------------
        -- Slave events
        ------------------------------------
        --
        -- Sources
        --
        Extraterrestrial ProcessSourcesCompleted (Ok _) ->
            Extraterrestrial.processSourcesCompleted model

        Extraterrestrial ReportProcessingError (Ok result) ->
            Extraterrestrial.reportProcessingError model result

        --
        -- Tracks
        --
        Extraterrestrial AddTracks (Ok result) ->
            Extraterrestrial.addTracks model result

        Extraterrestrial RemoveTracksByPath (Ok result) ->
            Extraterrestrial.removeTracksByPath model result

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



-- 🔥  ~  Debounce configurations


debounceStoreUserDataConfig : Debounce.Config Msg
debounceStoreUserDataConfig =
    { strategy = Debounce.later (2.5 * Time.second)
    , transform = DebounceCallbackStoreUserData
    }


debounceStoreLocalUserDataConfig : Debounce.Config Msg
debounceStoreLocalUserDataConfig =
    { strategy = Debounce.later (1 * Time.second)
    , transform = DebounceCallbackStoreLocalUserData
    }



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Time
          Time.every (1 * Time.minute) SetTimestamp

        -- Keyboard
        , Keyboard.downs KeydownMsg
        , Keyboard.ups KeyupMsg

        -- Shortcuts
        , Ports.shortcutPlayPause
            (\_ ->
                if model.console.isPlaying then
                    ConsoleMsg Console.Types.RequestPause
                else if Maybe.isNothing model.queue.activeItem then
                    QueueMsg Queue.Types.Shift
                else
                    ConsoleMsg Console.Types.RequestPlay
            )

        --
        , Ports.shortcutNext (\_ -> QueueMsg Queue.Types.Shift)
        , Ports.shortcutPrevious (\_ -> QueueMsg Queue.Types.Rewind)

        -- Ports
        , Ports.setIsTouchDevice SetIsTouchDevice
        , Ports.slaveEventResult handleSlaveResult
        , Ports.syncCompleted SyncCompleted
        , Ports.syncStarted (\_ -> SyncStarted)

        -- Children
        , Sub.map AbroadMsg <| Abroad.subscriptions model.abroad
        , Sub.map AlfredMsg <| Alfred.subscriptions model.alfred
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
