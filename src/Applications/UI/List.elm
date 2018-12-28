module UI.List exposing (Action, Item, view)

import Chunky exposing (..)
import Color exposing (Color)
import Color.Ext as Color
import Css exposing (px, solid)
import Html.Styled as Html exposing (Html, fromUnstyled)
import Html.Styled.Attributes exposing (css, style)
import Html.Styled.Events exposing (onClick)
import Tachyons.Classes as T
import UI.Kit
import VirtualDom



-- 🌳


type alias Action msg =
    { icon : Color -> Int -> VirtualDom.Node msg
    , msg : msg
    }


type alias Item msg =
    { label : String
    , actions : List (Action msg)
    }



-- ⛩


view : List (Item msg) -> Html msg
view =
    List.map item >> brick [ css listStyles ] [ T.lh_title ]



-----------------------------------------
-- ㊙️
-----------------------------------------


item : Item msg -> Html msg
item { label, actions } =
    brick
        [ css itemStyles ]
        [ T.flex, T.fw6, T.items_center, T.pv3 ]
        [ chunk
            [ T.flex_grow_1 ]
            [ Html.text label ]
        , chunk
            [ T.flex, T.items_center ]
            (List.map
                (\{ icon, msg } ->
                    brick
                        [ onClick msg, style "line-height" "0" ]
                        [ T.pointer ]
                        [ fromUnstyled (icon UI.Kit.colors.text 16) ]
                )
                actions
            )
        ]



-- 🖼


listStyles : List Css.Style
listStyles =
    [ Css.fontSize (px 13) ]


itemStyles : List Css.Style
itemStyles =
    [ Css.borderBottom3 (px 1) solid (Color.toElmCssColor UI.Kit.colors.subtleBorder) ]
