module UI.List exposing (view)

import Chunky exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes exposing (style)
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



-- 🚯


item : Item msg -> Html msg
item { label } =
    block
        [ style "border-bottom" ("1px solid " ++ Color.toCssString UI.Kit.colors.subtleBorder) ]
        []
        [ Html.text label ]
