module Navigation.Styles exposing (..)

import Color
import Color.Manipulate
import Css exposing (..)
import Css.Elements exposing (a, label, span, svg)
import Css.Media exposing (withMedia)
import Traits exposing (..)
import Variables exposing (colors, colorDerivatives, insulationWidth)


type Classes
    = ActiveLink
    | InsideNavigation
    | NonActiveLink
    | OutsideNavigation



-- 🦄


styles : List Snippet
styles =
    [ ------------------------------------------------------
      -- Outside
      ------------------------------------------------------
      class OutsideNavigation
        [ colorDerivatives.text
            |> Color.Manipulate.fadeOut 0.675
            |> cssColor
            |> color
        , fontSize (Css.rem 0.675)
        , letterSpacing (Css.em 0.0625)
        , margin3 (gr 7) auto zero
        , maxWidth insulationWidth
        , textAlign center
        , textTransform uppercase
        , whiteSpace noWrap
        , width (pct 100)

        --
        , descendants
            [ a
                [ display inlineBlock
                , marginRight (gr 4)

                --
                , withMedia [ iPhone6 ] [ marginRight (gr 6) ]
                , withMedia [ tablet ] [ marginRight (gr 9) ]

                --
                , lastChild
                    [ marginRight zero
                    ]
                ]
            , svg
                [ height (Css.em 1.475)
                , width (Css.em 1.475)
                ]
            , selector "g"
                [ fill currentColor
                ]
            , class ActiveLink
                [ colorDerivatives.text
                    |> Color.Manipulate.fadeOut 0.9
                    |> cssColor
                    |> borderBottom3 (px 1) solid
                , colorDerivatives.text
                    |> Color.Manipulate.fadeOut 0.45
                    |> cssColor
                    |> color
                ]
            ]
        ]

    ------------------------------------------------------
    -- Inside
    ------------------------------------------------------
    , class InsideNavigation
        [ backgroundColor (cssColor Color.white)
        , borderBottom3 (px 1) solid (hex "#eee")

        --
        , descendants
            [ a
                [ borderRight3 (px 1) solid (hex "#eee")
                , cursor pointer
                , display inlineBlock
                , fontSize (Traits.basem 13)
                , fontWeight (int 600)
                , height (gr 7)
                , letterSpacing (Css.em -0.0125)
                , lineHeight (gr 7)
                , padding2 zero (gr 3)

                --
                , lastChild
                    [ borderRight zero
                    ]

                --
                , children
                    [ span
                        [ display inlineBlock
                        , transform (translateY <| px 1)
                        ]
                    ]
                ]
            , svg
                [ height (Css.em 1.225)
                , transform (translateY <| px -1)
                , verticalAlign middle
                , width (Css.em 1.225)

                --
                , adjacentSiblings
                    [ label [ marginLeft (gr 1) ] ]
                ]
            , selector "g"
                [ fill currentColor
                ]
            , class ActiveLink
                [ color (cssColor colors.base0D)
                ]
            ]
        ]
    ]
