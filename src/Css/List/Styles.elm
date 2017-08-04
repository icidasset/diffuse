module List.Styles exposing (..)

import Color
import Css exposing (..)
import Css.Elements exposing (a, label, li, span, svg)
import Traits exposing (basem, cssColor, cssColorOpac, gr)
import Variables exposing (colors, colorDerivatives)


type Classes
    = ListWithActions
      -- Children
    | ListActions
    | SubtleListItem



-- 🦄


styles : List Snippet
styles =
    [ ------------------------------------------------------
      -- List with actions
      ------------------------------------------------------
      class ListWithActions
        [ fontSize (basem 13)
        , fontWeight (int 600)
        , listStyle none
        , margin zero
        , padding zero

        --
        , descendants
            [ li
                [ alignItems center
                , borderBottom3 (px 1) solid (cssColor <| Color.rgb 248 248 248)
                , borderTop3 (px 1) solid transparent
                , displayFlex
                , padding2 (gr 2) zero
                ]
            , label
                [ flex (int 1)
                , overflow hidden
                , textOverflow ellipsis
                , whiteSpace noWrap
                ]
            , Css.Elements.small
                [ backgroundColor (cssColorOpac 0.325 colors.base06)
                , borderRadius (px 2)
                , color (cssColor Color.white)
                , display inlineBlock
                , fontSize (Css.rem 0.675)
                , lineHeight (int 1)
                , marginRight (gr 3)
                , marginTop (basem -3)
                , overflow hidden
                , padding3 (em 0.3) (em 0) (em 0.25)
                , textAlign center
                , verticalAlign middle
                , width (em 1.6)
                ]
            , svg
                [ height (Css.em 1.225)
                , width (Css.em 1.225)
                ]
            ]
        ]

    ------------------------------------------------------
    -- Children
    ------------------------------------------------------
    , class ListActions
        [ displayFlex
        , lineHeight zero
        , children
            [ a
                [ cursor pointer
                , display inlineBlock
                , marginLeft (px 6)
                ]
            , span
                [ display inlineBlock
                , marginLeft (px 6)
                ]
            ]
        ]
    , class SubtleListItem
        [ color (cssColor colors.base04)
        ]
    ]
