module State exposing (..)

import Date
import List.Extra as List
import Navigation
import Response exposing (..)
import Task
import Time
import Types exposing (..)
import Users.Auth
import Utils exposing (do)
import Window


-- Children

import Console.State as Console
import Queue.State as Queue
import Routing.State as Routing
import Sources.State as Sources
import Tracks.State as Tracks


-- Children, Pt. 2

import Queue.Ports
import Queue.Types
import Queue.Utils
import Sources.Types
import Tracks.ContextMenu
import Tracks.Types
import Tracks.Utils


-- 💧


initialModel : ProgramFlags -> Navigation.Location -> Model
initialModel flags location =
    { authenticatedUser = flags.user
    , contextMenu = Nothing
    , showLoadingScreen = True

    ------------------------------------
    -- Time
    ------------------------------------
    , timestamp = Date.fromTime 0

    ------------------------------------
    -- Children
    ------------------------------------
    , console = Console.initialModel
    , queue = Queue.initialModel flags
    , routing = Routing.initialModel location
    , sources = Sources.initialModel flags
    , tracks = Tracks.initialModel flags
    }


initialCommands : ProgramFlags -> Navigation.Location -> Cmd Msg
initialCommands flags _ =
    Cmd.batch
        [ -- Time
          Task.perform SetTimestamp Time.now

        -- Children
        , Console.initialCommands
        , Queue.initialCommands
        , Routing.initialCommands
        , Sources.initialCommands
        , Tracks.initialCommands flags.tracks
        ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Authenticate ->
            (!)
                model
                [ Users.Auth.authenticate () ]

        ClickAway ->
            (!)
                { model | contextMenu = Nothing }
                []

        HideLoadingScreen ->
            (!)
                { model | showLoadingScreen = False }
                []

        SignOut ->
            (!)
                { model | authenticatedUser = Nothing }
                [ Users.Auth.deauthenticate ()
                , Navigation.modifyUrl "/"
                ]

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
        ConsoleMsg sub ->
            Console.update sub model.console
                |> mapModel (\x -> { model | console = x })

        QueueMsg sub ->
            Queue.update sub model.queue
                |> mapModel (\x -> { model | queue = x })

        RoutingMsg sub ->
            Routing.update sub model.routing
                |> mapModel (\x -> { model | routing = x })

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
                    |> Tracks.Types.IdentifyActiveTrack
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

        ShowTrackContextMenu ( index, mousePos ) ->
            let
                maybeIdentifiedTrack =
                    index
                        |> String.toInt
                        |> Result.toMaybe
                        |> Maybe.andThen (\idx -> List.getAt idx model.tracks.collection.exposed)

                contextMenu =
                    maybeIdentifiedTrack
                        |> Maybe.map Tracks.ContextMenu.trackMenu
                        |> Maybe.map (\fn -> fn mousePos)
            in
                (!)
                    { model | contextMenu = contextMenu }
                    []

        ShowViewMenu ->
            case model.contextMenu of
                Just _ ->
                    (!) { model | contextMenu = Nothing } []

                Nothing ->
                    (!) model [ Task.perform ShowViewMenuWithWindow Window.size ]

        ShowViewMenuWithWindow windowSize ->
            let
                position =
                    { x = windowSize.width // 2
                    , y = windowSize.height // 2
                    }
            in
                (!)
                    { model | contextMenu = Just (Tracks.ContextMenu.viewMenu position) }
                    []

        ToggleFavourite index ->
            (!)
                model
                [ index
                    |> Tracks.Types.ToggleFavourite
                    |> TracksMsg
                    |> do
                ]

        ------------------------------------
        -- Other
        ------------------------------------
        NoOp ->
            (!) model []



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Time
          Time.every (1 * Time.minute) SetTimestamp

        -- Children
        , Sub.map ConsoleMsg <| Console.subscriptions model.console
        , Sub.map QueueMsg <| Queue.subscriptions model.queue
        , Sub.map SourcesMsg <| Sources.subscriptions model.sources
        , Sub.map TracksMsg <| Tracks.subscriptions model.tracks
        ]
