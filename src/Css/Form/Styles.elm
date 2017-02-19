module Form.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Traits exposing (defaultFont, gr)
import Variables exposing (borderRadiuses, colorDerivatives)


type Classes
    = InputBox
    | Intro
    | SelectBox



-- 🦄


styles : List Snippet
styles =
    [ form
        [ descendants formStyles ]
    ]


formStyles : List Snippet
formStyles =
    [ class Intro
        [ fontSize (Css.em 0.95)
        , fontStyle italic
        , lineHeight (num 1.55)
        , marginBottom (gr 6)
        , opacity (num 0.55)
        ]
      ------------------------------------------------------
      -- Label
      ------------------------------------------------------
    , label
        [ display block
        , fontSize (Css.em 0.7)
        , fontWeight bold
        , letterSpacing (Css.em -0.0375)
        , marginBottom (px 5)
        , textTransform uppercase
        ]
      ------------------------------------------------------
      -- Select
      ------------------------------------------------------
    , select
        [ inputStyles
        , singleLineInputStyles
          --
        , property "-webkit-appearance" "none"
        , property "-moz-appearance" "none"
        , property "appearance" "none"
        ]
    , class SelectBox
        [ boxStyles
          --
        , position relative
          --
        , after
            [ marginTop (px -2)
            , position absolute
            , right (gr 3)
            , top (pct 50)
            , transforms [ translateY <| pct -50, rotate (deg -45) ]
              --
            , property "content" "'∟'"
            ]
        ]
      ------------------------------------------------------
      -- Input
      ------------------------------------------------------
    , input
        [ inputStyles
        , singleLineInputStyles
          --
        , border zero
        , borderBottom3 (px 1) solid (hex colorDerivatives.inputBorder)
        , borderRadius zero
        , padding zero
        ]
    , class InputBox
        [ boxStyles
        ]
    ]



-- Mixins


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
        , border3 (px 1) solid (hex colorDerivatives.inputBorder)
        , borderRadius borderRadiuses.smallElements
        , boxSizing borderBox
        , display block
        , width (pct 100)
          --
        , focus
            [ borderColor (hex colorDerivatives.focusBorder)
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
