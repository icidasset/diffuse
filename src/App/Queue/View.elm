module Queue.View exposing (..)

import Html exposing (Html, text)
import Types exposing (Model, Msg(..))


-- 🍯


entry : Model -> Html Msg
entry _ =
    text "Queue"
