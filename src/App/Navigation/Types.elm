module Navigation.Types exposing (..)

import Color exposing (Color)
import Svg exposing (Svg)


-- ⚗️


type Icon msg
    = Icon (Color -> Int -> Svg msg)


type Label
    = Label LabelType


type LabelType
    = Hidden String
    | Shown String
