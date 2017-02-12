module App exposing (..)

import Navigation
import Routing.Logic
import State
import Types exposing (..)
import View


main : Program ProgramFlags Model Msg
main =
    Navigation.programWithFlags
        router
        { init = init
        , view = View.entry
        , update = State.update
        , subscriptions = State.subscriptions
        }


init : ProgramFlags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    ( State.initialModel flags location
    , State.initialCommands flags location
    )


router : Navigation.Location -> Msg
router location =
    RoutingMsg (Routing.Logic.locationToMessage location)
