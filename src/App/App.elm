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
    let
        initialPage =
            Routing.Logic.locationToPage location

        origin =
            location.origin
    in
        ( State.initialModel flags initialPage origin
        , State.initialCommand flags initialPage
        )


router : Navigation.Location -> Msg
router location =
    RoutingMsg (Routing.Logic.locationToMessage location)
