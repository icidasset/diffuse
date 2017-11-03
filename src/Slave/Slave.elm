module Slave exposing (main)

import Slave.Types exposing (..)
import Slave.State as State


main : Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = State.update
        , subscriptions = State.subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( State.initialModel
    , State.initialCommand
    )
