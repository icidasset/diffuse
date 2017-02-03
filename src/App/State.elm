module State exposing (..)

import Firebase.Auth
import Navigation
import Response exposing (..)
import Types exposing (..)


-- Children

import Routing.State as Routing


-- 💧


initialModel : ProgramFlags -> Navigation.Location -> Model
initialModel flags location =
    { authenticatedUser = flags.user
    , showLoadingScreen = False
    , ------------------------------------
      -- Children
      ------------------------------------
      routing = Routing.initialModel location
    }


initialCommands : ProgramFlags -> Navigation.Location -> Cmd Msg
initialCommands _ _ =
    Cmd.batch
        [ Cmd.map RoutingMsg Routing.initialCommands
        ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Authenticate ->
            ( model, Firebase.Auth.authenticate () )

        ------------------------------------
        -- Children
        ------------------------------------
        RoutingMsg sub ->
            Routing.update sub model.routing
                |> mapModel (\x -> { model | routing = x })
                |> mapCmd RoutingMsg
