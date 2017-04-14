module List.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (label, li)
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
            ]
        ]

    ------------------------------------------------------
    -- Children
    ------------------------------------------------------
    , class ListActions
        [ displayFlex
        ]
    ]
