module State exposing (..)

import Navigation
import Response exposing (..)
import Routing.State as Routing
import Types exposing (..)


-- Initial


initialModel : ProgramFlags -> Navigation.Location -> Model
initialModel flags location =
    { backgroundImage = "images/Background/1.jpg"
    , routing = Routing.initialModel location
    }


initialCommands : ProgramFlags -> Navigation.Location -> Cmd Msg
initialCommands _ _ =
    Cmd.batch
        [ Cmd.map RoutingMsg Routing.initialCommands ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RoutingMsg sub ->
            Routing.update sub model.routing
                |> mapModel (\x -> { model | routing = x })
                |> mapCmd RoutingMsg
