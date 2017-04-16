module Tracks.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Traits exposing (cssColor, gr)
import Variables exposing (colorDerivatives)


type Classes
    = TracksContainer
    | TracksChild
    | TracksNavigation
    | TracksTable



-- 🦄


styles : List Snippet
styles =
    [ class TracksContainer
        [ displayFlex
        , flexDirection column
        , overflow hidden
        , width (pct 100)
        ]
    , class TracksChild
        [ flex (int 1)
        , overflow scroll
        , property "user-select" "none"
        , position relative
        , zIndex (int 2)
        ]

    ------------------------------------------------------
    -- Navigation
    ------------------------------------------------------
    , class TracksNavigation
        [ boxShadow5 zero zero (px 10) (px 1) (Css.rgba 0 0 0 0.05)
        , displayFlex
        , position relative
        , zIndex (int 1)

        --
        , descendants
            [ form
                [ borderBottom3 (px 1) solid (cssColor colorDerivatives.subtleBorder)
                , borderRight3 (px 1) solid (cssColor colorDerivatives.subtleBorder)
                , flex (int 1)
                , position relative
                ]
            , input
                [ borderBottom zero
                , borderTop3 (px 1) solid transparent
                , fontSize (Traits.basem 14)
                , padding2 zero (gr 2)
                , paddingLeft (gr 6)
                , position relative
                , zIndex (int 1)

                --
                , focus
                    [ borderTopColor transparent
                    ]
                ]
            , selector "input::placeholder"
                [ color (rgb 205 205 205)
                ]
            , (form << toa << children << toa << svg)
                [ left (gr 2)
                , marginTop (px 1)
                , position absolute
                , top (pct 50)
                , transform (translateY (pct -50))
                , zIndex (int 0)
                ]
            ]
        ]

    ------------------------------------------------------
    -- Table
    ------------------------------------------------------
    , class TracksTable
        [ borderCollapse collapse
        , color (hex "#444")
        , width (pct 100)

        --
        , descendants
            [ --
              -- <thead>
              --
              th
                [ backgroundColor (hex "#fff")
                , borderBottom3 (px 1) solid (cssColor colorDerivatives.subtleBorder)
                , borderLeft3 (px 1) solid (cssColor colorDerivatives.subtleBorder)
                , color (hex "#CFCFCF")
                , cursor pointer
                , fontSize (Css.rem 0.65)
                , paddingLeft (gr 2)
                , paddingTop (px 3)
                , position relative
                , textAlign left
                ]
            , (th << toa << firstChild)
                [ borderLeft zero
                , width (gr 4)
                ]
            , (th << toa << descendants << toa << svg)
                [ position absolute
                , right (gr 1)
                , top (pct 50)
                , transform (translateY (pct -50))
                ]

            --
            -- <tbody>
            --
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
                , property "padding-bottom" "calc(.375rem + 1px)"
                , textOverflow ellipsis
                , whiteSpace noWrap
                ]
            , (td << toa << descendants << toa << svg)
                [ display inlineBlock
                , verticalAlign textBottom
                ]

            -- <tbody> Add some extra space on top
            , (tr << toa << firstChild << toa << descendants << toa << td)
                [ paddingTop (gr 2)
                ]

            -- <tbody> And on the bottom
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
