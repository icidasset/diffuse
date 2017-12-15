module ContextMenu.StylesOld exposing (..)

import Css exposing (..)
import Css.Elements exposing (a, svg)
import Traits exposing (..)


type Classes
    = ContextMenu



-- 🦄


styles : List Snippet
styles =
    [ class ContextMenu
        [ descendants
            [ a
                [ -- borderBottom3 (px 1) solid (rgba 0 0 0 0.075)
                  cursor pointer
                , display block
                , lineHeight (num 1.35)
                , overflow hidden
                , padding2 (gr 2) (gr 3)
                , paddingRight (gr 5)
                , textOverflow ellipsis
                , whiteSpace noWrap

                --
                , lastChild
                    [ borderBottom zero
                    ]
                ]
            , svg
                [ marginRight (gr 2)
                , marginTop (px -2)
                , opacity (num 0.5)
                , verticalAlign middle
                ]
            ]
        ]
    ]
