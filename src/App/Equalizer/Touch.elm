module Equalizer.Touch exposing (..)

{-| Functions for dealing with touch events.
-}

import Equalizer.Types exposing (..)
import Mouse
import Json.Decode as Json exposing (Decoder, field, map2, int)
import Types as TopLevel exposing (Msg(EqualizerMsg))


start : Knob -> Decoder Equalizer.Types.Msg
start knobType =
    positionDecoder
        |> Json.map (ActivateKnob knobType)
        |> Json.at [ "touches", "0" ]


move : Decoder TopLevel.Msg
move =
    positionDecoder
        |> Json.map (AdjustKnob >> EqualizerMsg)
        |> Json.at [ "touches", "0" ]


end : Decoder TopLevel.Msg
end =
    positionDecoder
        |> Json.map (DeactivateKnob >> EqualizerMsg)
        |> Json.at [ "changedTouches", "0" ]



-- Decoding


positionDecoder : Decoder Mouse.Position
positionDecoder =
    map2 Mouse.Position
        (field "clientX" int)
        (field "clientY" int)
