module Form.Mixins exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Traits exposing (cssColor, defaultFont, gr)
import Variables exposing (borderRadiuses, colorDerivatives)


boxStyles : Mixin
boxStyles =
    mixin
        [ marginBottom (gr 6)
        ]


inputStyles : Mixin
inputStyles =
    mixin
        [ defaultFont

        --
        , backgroundColor transparent
        , border3 (px 1) solid (cssColor colorDerivatives.inputBorder)
        , borderRadius borderRadiuses.smallElements
        , boxSizing borderBox
        , color (cssColor colorDerivatives.text)
        , display block
        , fontSize (Traits.basem 15)
        , width (pct 100)

        --
        , focus
            [ borderColor (cssColor colorDerivatives.focusBorder)
            , outline none
            ]
        ]


singleLineInputStyles : Mixin
singleLineInputStyles =
    mixin
        [ height (gr 7)
        , lineHeight (gr 7)
        , padding2 zero (gr 2)
        ]
