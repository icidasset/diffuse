module UI.List exposing (view)

import Chunky exposing (..)
import Color
import Color.Ext as Color
import Css exposing (px, solid)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, style)
import Tachyons.Classes as T
import UI.Kit



-- 🌳


type alias Item msg =
    { label : String
    , actions : List (Html msg)
    }



-- ⛩


view : List (Item msg) -> Html msg
view =
    List.map item >> chunk [ T.f6, T.lh_copy ]



-----------------------------------------
-- ㊙️
-----------------------------------------


item : Item msg -> Html msg
item { label } =
    brick
        [ css itemStyles ]
        []
        [ Html.text label ]



-- 🖼


itemStyles : List Css.Style
itemStyles =
    [ Css.borderBottom3 (px 1) solid (Color.toElmCssColor UI.Kit.colors.subtleBorder) ]
