module Form.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (form, label, input, select, svg)
import Form.Mixins exposing (..)
import Traits exposing (basem, cssColor, cssColorOpac, gr)
import Variables exposing (colorDerivatives)


type Classes
    = InputBox
    | SelectBox



-- 🦄


styles : List Snippet
styles =
    [ form
        [ descendants formStyles ]
    ]


formStyles : List Snippet
formStyles =
    [ ------------------------------------------------------
      -- Label
      ------------------------------------------------------
      label
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
        , descendants
            [ svg
                [ fontSize (basem 20)
                , position absolute
                , property "pointer-events" "none"
                , property "top" "27.5%"
                , property "top" ("calc((100% - " ++ .value (gr 6) ++ ") / 2)")
                , right (gr 2)
                , transforms [ translateY <| pct -50 ]
                ]
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
        , borderBottom3 (px 1) solid (cssColor colorDerivatives.inputBorder)
        , borderRadius zero
        , padding zero

        --
        , pseudoElement "placeholder"
            [ color (cssColorOpac 0.375 colorDerivatives.text)
            ]
        ]
    , class InputBox
        [ boxStyles
        ]
    ]
