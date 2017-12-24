module Form.Styles exposing (Styles(..), styles)

import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Variables exposing (..)
import Variations exposing (Variations)


-- ⚗️


type Styles
    = Input
    | Label
    | Select



-- 🍯


styles : List (Style Styles Variations)
styles =
    [ -----------------------------------
      -- Input
      -----------------------------------
      style Input
        [ Border.bottom 1
        , Color.border colorDerivatives.inputBorder
        , Font.size (scaled -1)
        ]
    , -----------------------------------
      -- Label
      -----------------------------------
      style Label
        [ Font.size (scaled -3)
        , Font.uppercase
        , Font.weight 700
        ]
    ]
