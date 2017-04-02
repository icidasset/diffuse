module Navigation.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (a, span, svg)
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
        [ color (rgba 0 0 0 0.55)
        , fontSize (em 0.875)
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
                , display inlineBlock
                , fontSize (Css.rem 0.85)
                , height (gr 7)
                , letterSpacing (Css.em -0.0125)
                , lineHeight (gr 7)
                , padding2 zero (gr 3)
                ]
            , span
                [ display inlineBlock
                , transform (translateY <| px 1)
                ]
            , svg
                [ transform (translateY <| px -1)
                , verticalAlign middle
                ]
            ]
        ]
    ]
