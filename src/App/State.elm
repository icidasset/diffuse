module State exposing (..)

import Date
import Firebase.Auth
import List.Extra as List
import Navigation
import Response exposing (..)
import Task
import Time
import Types exposing (..)
import Utils exposing (do)


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


-- 💧


initialModel : ProgramFlags -> Navigation.Location -> Model
initialModel flags location =
    { authenticatedUser = flags.user
    , showLoadingScreen = False

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
                [ Firebase.Auth.authenticate () ]

        SignOut ->
            (!)
                { model | authenticatedUser = Nothing }
                [ Firebase.Auth.deauthenticate () ]

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
                [ maybeQueueItem
                    |> Maybe.map
                        (.track)
                    |> Maybe.map
                        (Queue.Utils.makeEngineItem
                            model.timestamp
                            model.sources.collection
                        )
                    |> Queue.Ports.activeQueueItemChanged
                ]

        CleanQueue ->
            (!)
                model
                [ model.tracks.searchResults
                    |> Queue.Types.Clean
                    |> QueueMsg
                    |> do
                ]

        FillQueue ->
            (!)
                model
                [ model.tracks.searchResults
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
                    |> Maybe.andThen (\idx -> List.getAt idx model.tracks.resultant)
                    |> Maybe.map (Queue.Types.InjectFirstAndPlay)
                    |> Maybe.map (QueueMsg)
                    |> Maybe.map (do)
                    |> Maybe.withDefault Cmd.none
                ]

        ProcessSources ->
            (!)
                model
                [ model.tracks.collection
                    |> Sources.Types.Process
                    |> SourcesMsg
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
