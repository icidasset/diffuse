module State exposing (..)

import Date
import Firebase.Auth
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


-- Children types

import Queue.Types
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
    }


initialCommands : ProgramFlags -> Navigation.Location -> Cmd Msg
initialCommands _ _ =
    Cmd.batch
        [ -- Time
          Task.perform SetTimestamp Time.now

        -- Children
        , Console.initialCommands
        , Queue.initialCommands
        , Routing.initialCommands
        , Sources.initialCommands
        ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Authenticate ->
            ( model, Firebase.Auth.authenticate () )

        ------------------------------------
        -- Time
        ------------------------------------
        SetTimestamp time ->
            let
                stamp =
                    Date.fromTime time

                queue =
                    model.queue

                sources =
                    model.sources
            in
                (!)
                    { model
                        | queue = { queue | timestamp = stamp }
                        , sources = { sources | timestamp = stamp }
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

        ------------------------------------
        -- Children, Pt. 2
        ------------------------------------
        FillQueue ->
            (!)
                model
                [ model.sources.collection
                    |> Queue.Types.Fill
                    |> QueueMsg
                    |> do
                ]

        ProcessSources ->
            (!)
                model
                [ model.queue.tracks
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
          Time.every (5 * Time.minute) SetTimestamp

        -- Children
        , Sub.map ConsoleMsg <| Console.subscriptions model.console
        , Sub.map QueueMsg <| Queue.subscriptions model.queue
        , Sub.map SourcesMsg <| Sources.subscriptions model.sources
        ]
