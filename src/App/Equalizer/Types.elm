module Equalizer.Types exposing (..)

import Mouse


-- Messages


type Msg
    = ActivateKnob Knob Mouse.Position
    | AdjustKnob Mouse.Position
    | DeactivateKnob
    | ResetKnob Knob



-- Model


type alias Model =
    { low : Float
    , mid : Float
    , high : Float
    , volume : Float

    -- Knob interactions
    , activeKnob : Maybe Knob
    , startingMousePosition : Mouse.Position
    }



-- Knobs


type Knob
    = Low
    | Mid
    | High
    | Volume
