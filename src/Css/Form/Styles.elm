module Form.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (form, label, input, select, svg)
import Form.Mixins exposing (..)
import Traits exposing (cssColor, gr, headerFont)
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
                [ position absolute
                , property "pointer-events" "none"
                , right (gr 2)
                , top (pct 50)
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
        ]
    , class InputBox
        [ boxStyles
        ]
    ]
