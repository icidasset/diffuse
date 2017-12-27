module Tracks.Styles exposing (..)

import Color
import Css exposing (..)
import Css.Elements exposing (..)
import Traits exposing (..)
import Variables exposing (colors, colorDerivatives)


type Classes
    = FavouritesOnly
    | NoTracksFound
    | NoTracksFoundUnderline
    | TracksContainer
    | TracksChild
    | TracksNavigation
    | TracksNavigationIcon
    | TracksNavigationIcons
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
        [ disableSelect
        , flex (int 1)
        , overflowX hidden
        , overflowY scroll
        , position relative
        , property "-webkit-overflow-scrolling" "touch"
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
            ]
        ]
    , class TracksNavigationIcon
        [ cursor pointer
        , lineHeight zero
        , marginTop (px 1)
        , position absolute
        , top (pct 50)
        , transform (translateY (pct -50))
        , zIndex (int 0)

        -- Search icon
        , nthOfType "1"
            [ left (gr 2) ]

        -- Other icons
        , nthOfType "2"
            [ right (gr 2)
            , zIndex (int 2)
            ]
        , nthOfType "3"
            [ right (gr 6)
            , zIndex (int 2)
            ]
        , nthOfType "4"
            [ right (gr 10)
            , zIndex (int 2)
            ]
        ]

    ------------------------------------------------------
    -- Table
    ------------------------------------------------------
    , class TracksTable
        [ borderCollapse collapse
        , color (hex "#444")
        , disableSelect
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
            , tbody
                [ before
                    [ color (cssColor Color.white)
                    , display block
                    , fontSize (px 0)
                    , lineHeight (px 2)
                    , property "content" "'x'"
                    ]
                , after
                    [ color (cssColor Color.white)
                    , display block
                    , fontSize (px 0)
                    , lineHeight (px 2)
                    , property "content" "'x'"
                    ]
                ]

            --
            , tr
                [ (cursor pointer)
                , (nthChild "2n" << toa << children << toa << td)
                    [ backgroundColor (hex "#fcfcfc") ]
                ]

            --
            , td
                [ fontSize (Traits.basem 12)
                , lineHeight (num 1.6)
                , maxWidth zero
                , overflow hidden
                , property "padding" "calc(.375rem + 1px) .75rem"
                , textOverflow ellipsis
                , whiteSpace noWrap
                ]

            -- <tbody> Modifiers
            , selector "tr[data-missing=\"t\"]"
                [ color (hex "#8f8f8f")
                ]
            , selector "tr[data-nowplaying=\"t\"] > td"
                [ backgroundColor (cssColor colors.base0D)
                , color (cssColor Color.white)
                ]
            , selector "tr[data-nowplaying=\"t\"] > td[data-favourite=\"f\"]"
                [ color (rgba 255 255 255 0.4)
                ]

            -- <tbody> Favourites
            , selector "tr > td[data-favourite]"
                [ color (hex "#dedede")
                , fontFamilies [ "'or-favourites'" ]
                , position relative
                , property "text-overflow" "initial"

                --
                , before
                    [ property "content" "\"f\""
                    , position relative
                    , top (px 1)
                    ]
                ]
            , selector "tr > td[data-favourite=\"t\"]"
                [ color (cssColor colors.base08)

                --
                , before
                    [ property "content" "'t'"
                    ]
                ]

            -- <tbody> Selected
            , selector "tr[data-selected=\"t\"] > td[data-favourite]:after"
                [ backgroundColor (cssColor colors.base08)
                , borderRadius (pct 50)
                , height (px 5)
                , position absolute
                , property "content" "''"
                , right (px 0)
                , top (pct 50)
                , transform (translateY <| pct -50)
                , width (px 5)
                ]
            ]
        ]

    --
    , (class FavouritesOnly
        << toa
        << descendants
        << toa
        << class TracksTable
        << toa
        << descendants
      )
        [ selector "tr > td[data-favourite]"
            [ color (hex "#dedede") ]
        , selector "tr[data-nowplaying=\"t\"] > td[data-favourite=\"t\"]"
            [ color (hex "#ffffff") ]
        ]
    ]



-- Utils


toa : a -> List a
toa =
    List.singleton
