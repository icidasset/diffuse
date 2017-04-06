module State exposing (..)

import Date
import Firebase.Auth
import Navigation
import Response exposing (..)
import Task
import Time
import Types exposing (..)


-- Children

import Queue.State as Queue
import Routing.State as Routing
import Sources.State as Sources
import Queue.Types
import Routing.Types
import Sources.Types


-- 💧


initialModel : ProgramFlags -> Navigation.Location -> Model
initialModel flags location =
    { authenticatedUser = flags.user
    , showLoadingScreen = False
    , ------------------------------------
      -- Time
      ------------------------------------
      timestamp = Date.fromTime 0
    , ------------------------------------
      -- Children
      ------------------------------------
      queue = Queue.initialModel flags.settings.queue
    , routing = Routing.initialModel location
    , sources = Sources.initialModel flags
    }


initialCommands : ProgramFlags -> Navigation.Location -> Cmd Msg
initialCommands _ _ =
    Cmd.batch
        [ -- Time
          Task.perform SetTimestamp Time.now

        -- Children
        , Cmd.map QueueMsg Queue.initialCommands
        , Cmd.map RoutingMsg Routing.initialCommands
        , Cmd.map SourcesMsg Sources.initialCommands
        ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Authenticate ->
            ( model, Firebase.Auth.authenticate () )

        -- Time
        SetTimestamp time ->
            let
                timestamp =
                    Date.fromTime time

                sources =
                    model.sources

                sources_ =
                    { sources | timestamp = timestamp }
            in
                (!)
                    { model | sources = sources_, timestamp = timestamp }
                    []

        ------------------------------------
        -- Children
        ------------------------------------
        QueueMsg sub ->
            updateQueue sub model

        RoutingMsg sub ->
            updateRouting sub model

        SourcesMsg sub ->
            updateSources sub model



-- Children boilerplate


updateQueue : Queue.Types.Msg -> Model -> ( Model, Cmd Msg )
updateQueue msg model =
    Queue.update msg model.queue
        |> mapModel (\x -> { model | queue = x })
        |> mapCmd QueueMsg


updateRouting : Routing.Types.Msg -> Model -> ( Model, Cmd Msg )
updateRouting msg model =
    Routing.update msg model.routing
        |> mapModel (\x -> { model | routing = x })
        |> mapCmd RoutingMsg


updateSources : Sources.Types.Msg -> Model -> ( Model, Cmd Msg )
updateSources msg model =
    Sources.update msg model.sources
        |> mapModel (\x -> { model | sources = x })
        |> mapCmd SourcesMsg



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Time
          Time.every (5 * Time.minute) SetTimestamp

        -- Children
        , Sub.map QueueMsg <| Queue.subscriptions model.queue
        , Sub.map SourcesMsg <| Sources.subscriptions model.sources
        ]
