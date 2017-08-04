module Tracks.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Traits exposing (cssColor, gr)
import Variables exposing (colors, colorDerivatives)


type Classes
    = FavouritesOnly
    | NoTracksFound
    | NoTracksFoundUnderline
    | TracksContainer
    | TracksChild
    | TracksNavigation
    | TracksNavigationIcon
    | TracksTable



-- 🦄


styles : List Snippet
styles =
    [ class TracksContainer
        [ borderRadius (px 3)
        , displayFlex
        , flexDirection column
        , overflow hidden
        , width (pct 100)
        ]
    , class TracksChild
        [ flex (int 1)
        , overflowX hidden
        , overflowY scroll
        , property "user-select" "none"
        , position relative
        , zIndex (int 2)
        ]
    , class NoTracksFound
        [ fontSize (Css.rem 0.925)
        , fontWeight (int 600)
        , left (pct 50)
        , opacity (num 0.75)
        , position absolute
        , textAlign center
        , top (pct 50)
        , transform (translate2 (pct -50) (pct -50))
        , zIndex (int 3)
        ]
    , class NoTracksFoundUnderline
        [ borderBottom3 (px 2) solid currentColor
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
            , class TracksNavigationIcon
                [ lineHeight zero
                , marginTop (px 1)
                , position absolute
                , top (pct 50)
                , transform (translateY (pct -50))
                , zIndex (int 0)

                -- Search icon
                , nthOfType "1"
                    [ left (gr 2) ]

                -- Clear icon
                , nthOfType "2"
                    [ cursor pointer
                    , right (gr 6)
                    , zIndex (int 2)
                    ]

                -- Favourites-only icon
                , nthOfType "3"
                    [ cursor pointer
                    , right (gr 2)
                    , zIndex (int 2)
                    ]
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
                [ fontSize (Css.rem 1)
                , position absolute
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

            -- <tbody> Favourites
            , selector "td[data-favourite]"
                [ color (hex "#dedede")
                , fontFamilies [ "Material Icons" ]
                , property "text-overflow" "initial"
                , before [ property "content" "'favorite_border'" ]
                ]
            , selector "td[data-favourite=\"t\"]"
                [ color (cssColor colors.base08)
                , before [ property "content" "'favorite'" ]
                ]

            -- <tbody> Modifiers
            , selector "tr[data-missing=\"t\"]"
                [ color (hex "#8f8f8f")
                ]
            , selector "tr[data-nowplaying=\"t\"]"
                [ color (cssColor colors.base0B)
                ]
            , selector "tr[data-nowplaying=\"t\"] td[data-favourite=\"t\"]"
                [ color (cssColor colors.base0B)
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
    , (class FavouritesOnly
        << toa
        << descendants
        << toa
        << selector "td[data-favourite]"
      )
        [ color (hex "#dedede")
        ]
    ]



-- Utils


toa : a -> List a
toa =
    List.singleton
