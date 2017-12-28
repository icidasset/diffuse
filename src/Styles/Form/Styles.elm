module Form.Styles exposing (Styles(..), styles)

import Color
import Color.Ext as Color
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



-- 🍯


styles : List (Style Styles Variations)
styles =
    [ -----------------------------------
      -- Input
      -----------------------------------
      style Input
        [ Border.bottom 1
        , Color.background (Color.rgba 0 0 0 0)
        , Color.border colorDerivatives.inputBorder
        , Color.placeholder (Color.setAlpha 0.375 colorDerivatives.text)
        , Font.size (scaled -1)

        --
        , focus
            [ Color.border colorDerivatives.inputBorder
            , Style.prop "box-shadow" "none"
            ]
        ]

    -----------------------------------
    -- Label
    -----------------------------------
    , style Label
        [ Font.size (scaled -3)
        , Font.uppercase
        , Font.weight 700
        ]
    ]
