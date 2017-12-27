module Form.Mixins exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Traits exposing (cssColor, defaultFont, gr)
import Variables exposing (colorDerivatives)


boxStyles : Style
boxStyles =
    batch
        [ margin2 zero auto
        , paddingBottom (gr 6)
        ]


inputStyles : Style
inputStyles =
    batch
        [ defaultFont

        --
        , backgroundColor transparent
        , border zero
        , borderBottom3 (px 1) solid (cssColor colorDerivatives.inputBorder)
        , borderRadius zero
        , boxSizing borderBox
        , color (cssColor colorDerivatives.text)
        , display block
        , fontSize (Traits.basem 15)
        , width (pct 100)

        --
        , focus
            [ outline none
            ]
        ]


singleLineInputStyles : Style
singleLineInputStyles =
    batch
        [ height (gr 7)
        , lineHeight (gr 7)
        , padding zero
        ]
