module App exposing (..)

import Navigation
import Routing.Logic
import State
import Types exposing (..)
import View


main : Program Never Model Msg
main =
    Navigation.program
        router
        { init = init
        , view = View.entry
        , update = State.update
        , subscriptions = State.subscriptions
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        initialPage =
            Routing.Logic.locationToPage location
    in
        ( State.initialModel initialPage
        , State.initialCommands initialPage
        )


router : Navigation.Location -> Msg
router location =
    RoutingMsg (Routing.Logic.locationToMessage location)
