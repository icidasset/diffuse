module Html.Ext exposing (ifEnterKey, onClickStopPropagation, onDoubleTap, onEnterKey, onTap, onTapPreventDefault, onTapStopPropagation)

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on, preventDefaultOn, stopPropagationOn)
import Json.Decode as Json


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    stopPropagationOn "click" (Json.succeed ( msg, True ))


onDoubleTap : msg -> Attribute msg
onDoubleTap msg =
    on "dbltap" (Json.succeed msg)


onEnterKey : msg -> Attribute msg
onEnterKey msg =
    on "keydown" (Json.andThen (ifEnterKey msg) keyCode)


ifEnterKey : msg -> Int -> Json.Decoder msg
ifEnterKey msg key =
    case key of
        13 ->
            Json.succeed msg

        _ ->
            Json.fail "Another key, that isn't enter, was pressed"


onTap : msg -> Attribute msg
onTap msg =
    on "tap" (Json.succeed msg)


onTapPreventDefault : msg -> Attribute msg
onTapPreventDefault msg =
    preventDefaultOn "tap" (Json.succeed ( msg, True ))


onTapStopPropagation : msg -> Attribute msg
onTapStopPropagation msg =
    stopPropagationOn "tap" (Json.succeed ( msg, True ))
