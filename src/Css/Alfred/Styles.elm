module Alfred.Styles exposing (..)

import Color
import Css exposing (..)
import Css.Elements exposing (input, li)
import Traits exposing (cssColor, gr)
import Variables exposing (..)


type Classes
    = Alfred
    | AlfredInput
    | AlfredMessage
    | AlfredResults



-- 🦄


styles : List Snippet
styles =
    [ ------------------------------------------------------
      -- 🧢
      ------------------------------------------------------
      class Alfred
        [ boxSizing borderBox
        , color (rgb 255 255 255)
        , left (pct 50)
        , maxWidth (px 500)
        , padding2 zero (gr 3)
        , position fixed
        , top (pct 17.5)
        , transform (translate2 (pct -50) (gr -7))
        , width (pct 100)
        , zIndex (int 901)
        ]

    ------------------------------------------------------
    -- Input
    ------------------------------------------------------
    , class AlfredInput
        [ backgroundColor (rgb 255 255 255)
        , borderRadius (px 3)
        , property "box-shadow" "0 1px 3px 0 rgba(0, 0, 0, 0.225), 0 3px 15px 0 rgba(0, 0, 0, 0.1)"

        --
        , descendants
            [ input inputStyles ]
        ]

    ------------------------------------------------------
    -- Message
    ------------------------------------------------------
    , class AlfredMessage
        [ boxSizing borderBox
        , fontSize (Css.rem 0.95)
        , fontStyle italic
        , left (pct 50)
        , overflow hidden
        , padding2 zero (gr 3)
        , position absolute
        , top (gr -10)
        , textAlign center
        , textOverflow ellipsis
        , transform (translateX (pct -50))
        , whiteSpace noWrap
        , width (pct 100)
        ]

    ------------------------------------------------------
    -- Results
    ------------------------------------------------------
    , class AlfredResults
        [ backgroundColor (rgba 0 0 0 0.75)
        , borderRadius (px 3)
        , color (cssColor Color.white)
        , fontSize (Css.rem 0.925)
        , listStyle none
        , margin3 (gr 1) zero zero
        , maxHeight (vh 49.5)
        , overflowX hidden
        , overflowY scroll
        , padding zero

        --
        , [ li resultStyles ]
            |> descendants

        --
        , [ li resultOnHoverStyles ]
            |> descendants
            |> List.singleton
            |> hover
        ]
    ]



-- Input


inputStyles : List Style
inputStyles =
    [ backgroundColor transparent
    , borderWidth zero
    , fontSize (Css.rem 1.5)
    , height auto
    , letterSpacing (em -0.0175)
    , lineHeight (int 1)
    , padding2 (gr 3) (gr 3)

    --
    , focus
        [ outline zero ]
    ]



-- Results


resultStyles : List Style
resultStyles =
    [ borderBottom3 (px 1) solid (rgba 255 255 255 0.15)
    , cursor pointer
    , lineHeight (num 1.2)
    , overflow hidden
    , padding2 (gr 2) (gr 3)
    , textOverflow ellipsis
    , whiteSpace noWrap

    --
    , lastChild
        [ borderBottomWidth zero ]
    ]


resultOnHoverStyles : List Style
resultOnHoverStyles =
    [ important (color inherit)
    , important (fontWeight normal)

    --
    , hover
        [ important (color <| cssColor colors.base0B)
        , important (fontWeight bold)
        ]
    ]
