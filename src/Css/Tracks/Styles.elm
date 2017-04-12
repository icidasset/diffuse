module Tracks.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (svg, table, td, th, tr)
import Traits exposing (cssColor, gr)
import Variables exposing (colorDerivatives)


type Classes
    = TracksContainer
    | TracksNavigation
    | TracksTable
    | TracksTableContainer



-- 🦄


styles : List Snippet
styles =
    [ class TracksContainer
        [ displayFlex
        , flexDirection column
        , overflow hidden
        , width (pct 100)
        ]

    ------------------------------------------------------
    -- Navigation
    ------------------------------------------------------
    , class TracksNavigation
        [ boxShadow5 zero zero (px 10) (px 1) (Css.rgba 0 0 0 0.05)
        , position relative
        , zIndex (int 1)
        ]

    ------------------------------------------------------
    -- Table
    ------------------------------------------------------
    , class TracksTableContainer
        [ flex (int 1)
        , overflow scroll
        , position relative
        , zIndex (int 2)

        --
        , property "user-select" "none"
        ]
    , class TracksTable
        [ borderCollapse collapse
        , color (hex "#444")
        , width (pct 100)

        --
        , descendants
            [ th
                [ backgroundColor (hex "#fff")
                , borderBottom3 (px 1) solid (cssColor colorDerivatives.subtleBorder)
                , borderLeft3 (px 1) solid (cssColor colorDerivatives.subtleBorder)
                , color (hex "#CFCFCF")
                , fontSize (Css.rem 0.65)
                , paddingLeft (gr 2)
                , paddingTop (px 3)
                , textAlign left
                ]
            , (th << toa << firstChild)
                [ borderLeft zero
                , width (gr 4)
                ]
            , th [ cursor pointer ]
            , tr [ cursor pointer ]
            , (tr << toa << nthChild "2n" << toa << descendants << toa << td)
                [ backgroundColor (hex "#fcfcfc")
                ]
            , td
                [ fontSize (Traits.basem 12)
                , lineHeight (num 1.6)
                , maxWidth zero
                , overflow hidden
                , padding2 (gr 1) (gr 2)
                , textOverflow ellipsis
                , whiteSpace noWrap

                --
                , property "padding-bottom" "calc(.375rem + 1px)"
                ]
            , (td << toa << descendants << toa << svg)
                [ display inlineBlock
                , verticalAlign textBottom
                ]

            -- Add some extra space on top
            , (tr << toa << firstChild << toa << descendants << toa << td)
                [ paddingTop (gr 2)
                ]

            -- And on the bottom
            , (tr << toa << lastChild << toa << descendants << toa << td)
                [ paddingBottom (gr 2)
                ]
            ]
        ]
    ]



-- Utils


toa : a -> List a
toa =
    List.singleton
