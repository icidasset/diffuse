module List.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (a, label, li, span)
import Traits exposing (basem, cssColor, gr)
import Variables exposing (colorDerivatives)


type Classes
    = ListWithActions
      -- Children
    | ListActions



-- 🦄


styles : List Snippet
styles =
    [ ------------------------------------------------------
      -- List with actions
      ------------------------------------------------------
      class ListWithActions
        [ fontSize (basem 13)
        , listStyle none
        , margin zero
        , padding zero

        --
        , descendants
            [ li
                [ alignItems center
                , borderBottom3 (px 1) dashed (cssColor colorDerivatives.subtleBorder)
                , borderTop3 (px 1) solid transparent
                , displayFlex
                , padding2 (gr 2) zero
                ]
            , label
                [ flex (int 1)
                ]
            , Css.Elements.small
                [ color (hex "#afafaf")
                , display inlineBlock
                , fontSize (pct 87.5)
                , marginRight (gr 2)
                ]
            ]
        ]

    ------------------------------------------------------
    -- Children
    ------------------------------------------------------
    , class ListActions
        [ displayFlex
        , children
            [ a
                [ cursor pointer
                , display inlineBlock
                , marginLeft (px 2)
                ]
            , span
                [ display inlineBlock
                , marginLeft (px 2)
                ]
            ]
        ]
    ]
