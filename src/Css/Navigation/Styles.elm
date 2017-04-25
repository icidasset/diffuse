module Navigation.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (a, label, span, svg)
import Traits exposing (gr)
import Variables exposing (insulationWidth)


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
        [ color (rgba 0 0 0 0.4)
        , fontSize (em 0.85)
        , margin3 (gr 7) auto zero
        , maxWidth insulationWidth
        , textAlign center
        , width (pct 100)

        --
        , descendants
            [ a
                [ display inlineBlock
                , marginRight (gr 9)

                --
                , lastChild
                    [ marginRight zero
                    ]
                ]
            , class ActiveLink
                [ fontWeight (int 700)
                ]
            ]
        ]

    ------------------------------------------------------
    -- Inside
    ------------------------------------------------------
    , class InsideNavigation
        [ borderBottom3 (px 1) solid (hex "#eee")

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
                [ transform (translateY <| px -1)
                , verticalAlign middle

                --
                , adjacentSiblings
                    [ label [ marginLeft (gr 1) ] ]
                ]
            ]
        ]
    ]
