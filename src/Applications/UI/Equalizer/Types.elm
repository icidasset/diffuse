module UI.Equalizer.Types exposing (..)

import Coordinates exposing (Coordinates)
import Equalizer exposing (..)



-- 🌳


type alias Model =
    { low : Float
    , mid : Float
    , high : Float
    , volume : Float

    --
    , activeKnob : Maybe Knob
    , startCoordinates : Coordinates
    }
