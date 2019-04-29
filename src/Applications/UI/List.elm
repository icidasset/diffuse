module UI.List exposing (Action, Item, view)

import Chunky exposing (..)
import Classes as C
import Color exposing (Color)
import Color.Ext as Color
import Css exposing (px, solid)
import Html.Events.Extra.Mouse as Mouse exposing (onClick)
import Html.Styled as Html exposing (Html, fromUnstyled)
import Html.Styled.Attributes as Attributes exposing (css, style, title)
import Tachyons.Classes as T
import UI.Kit
import VirtualDom



-- 🌳


type alias Action msg =
    { icon : Color -> Int -> VirtualDom.Node msg
    , msg : Mouse.Event -> msg
    , title : String
    }


type alias Item msg =
    { label : Html msg
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
        [ -- Label
          --------
          chunk
            [ T.flex_grow_1 ]
            [ label ]

        -- Actions
        ----------
        , chunk
            [ T.flex, T.items_center ]
            (List.map
                (\action ->
                    brick
                        [ Attributes.fromUnstyled (onClick action.msg)
                        , title action.title
                        ]
                        [ C.lh_0
                        , T.ml2
                        , T.pointer
                        ]
                        [ fromUnstyled (action.icon UI.Kit.colors.text 16) ]
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
    [ Css.borderBottom3 (px 1) solid (Color.toElmCssColor UI.Kit.colors.verySubtleBorder) ]
