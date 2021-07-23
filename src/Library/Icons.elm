module Icons exposing (..)

import Chunky exposing (slaby)
import Html
import Material.Icons.Types exposing (Coloring)
import VirtualDom



-- 🌳


type alias Icon msg =
    Int -> Coloring -> VirtualDom.Node msg



-- 🔱


wrapped : List String -> Icon msg -> Int -> Coloring -> VirtualDom.Node msg
wrapped classes icon size coloring =
    coloring
        |> icon size
        |> slaby Html.span [] classes
