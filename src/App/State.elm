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
    , sources = Sources.initialModel
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
            (!) { model | timestamp = Date.fromTime time } []

        ------------------------------------
        -- Children
        ------------------------------------
        QueueMsg sub ->
            Queue.update sub model.queue
                |> mapModel (\x -> { model | queue = x })
                |> mapCmd QueueMsg

        RoutingMsg sub ->
            Routing.update sub model.routing
                |> mapModel (\x -> { model | routing = x })
                |> mapCmd RoutingMsg

        SourcesMsg sub ->
            Sources.update sub model.sources
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
        ]
