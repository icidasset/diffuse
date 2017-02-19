module HorizontalNavigation.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (a, span)
import Traits exposing (gr)


type Classes
    = HorizontalNavigation



-- 🦄


styles : List Snippet
styles =
    [ class HorizontalNavigation
        [ borderBottom3 (px 1) solid (hex "#eee")
        , descendants
            [ a
                [ borderRight3 (px 1) solid (hex "#eee")
                , display inlineBlock
                , fontSize (Css.rem 0.85)
                , height (gr 7)
                , lineHeight (gr 7)
                , padding2 zero (gr 3)
                ]
            , span
                [ letterSpacing (Css.em -0.0125)
                , position relative
                , top (px 1)
                ]
            ]
        ]
    ]
