module UI.Common.Types exposing (..)

import Debouncer.Basic as Debouncer
import UI.Types exposing (Manager, Model, Msg)



-- 🌳


type alias DebounceManager =
    (Msg -> Model -> ( Model, Cmd Msg )) -> Debouncer.Msg Msg -> Manager
