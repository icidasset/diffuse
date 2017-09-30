module Form.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (form, label, input, select, svg)
import Css.Media exposing (withMedia)
import Form.Mixins exposing (..)
import Traits exposing (..)
import Variables exposing (colorDerivatives)


type Classes
    = HalfWidthForm
    | InputBox
    | SelectBox



-- 🦄


styles : List Snippet
styles =
    [ form
        [ descendants formStyles ]

    ------------------------------------------------------
    -- Wrappers 🎁
    ------------------------------------------------------
    , class HalfWidthForm
        [ -- This kind of form should have a width of 50% on
          -- tablet screens or bigger
          withMedia
            [ tablet ]
            [ width (pct 50) ]
        ]
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
                , right zero
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
        , pseudoElement "placeholder"
            [ color (cssColorOpac 0.375 colorDerivatives.text)
            ]
        ]
    , class InputBox
        [ boxStyles
        ]

    ------------------------------------------------------
    -- File input
    ------------------------------------------------------
    , selector "input[type='file']"
        [ height (px 0.1)
        , opacity zero
        , overflow hidden
        , position absolute
        , width (px 0.1)
        , zIndex (int -1)
        ]
    ]
