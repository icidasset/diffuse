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
    InternalModel Settings


type alias InternalModel extension =
    { extension
        | activeKnob : Maybe Knob
        , startingMousePosition : Mouse.Position
    }


type alias Settings =
    { low : Float
    , mid : Float
    , high : Float
    , volume : Float
    }



-- Knobs


type Knob
    = Low
    | Mid
    | High
    | Volume


type alias KnobWithValue =
    { knob : String
    , value : Float
    }
