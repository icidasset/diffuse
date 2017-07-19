module ContextMenu.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (a, svg)
import Traits exposing (..)


type Classes
    = ContextMenu



-- 🦄


styles : List Snippet
styles =
    [ class ContextMenu
        [ backgroundColor (rgb 255 255 255)
        , fontSize (Css.rem 0.8)
        , position absolute
        , property "box-shadow" "0 1px 3px 0 rgba(0, 0, 0, 0.225), 0 3px 15px 0 rgba(0, 0, 0, 0.1)"
        , transform (translate2 (pct -50) (pct -50))
        , zIndex (int 1000)

        --
        , descendants
            [ a
                [ borderBottom3 (px 1) solid (rgba 0 0 0 0.075)
                , cursor pointer
                , display block
                , lineHeight (num 1.35)
                , padding2 (gr 2) (gr 3)

                --
                , lastChild
                    [ borderBottom zero
                    ]
                ]
            , svg
                [ marginRight (gr 1)
                , marginTop (px -2)
                , paddingRight (px 1)
                , verticalAlign middle
                ]
            ]
        ]
    ]
