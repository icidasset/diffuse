module ContextMenu exposing (ContextMenu(..), Item(..))

import Color exposing (Color)
import Coordinates exposing (Coordinates)
import Material.Icons exposing (Coloring(..))
import Svg exposing (Svg)



-- 🌳


type ContextMenu msg
    = ContextMenu (List (Item msg)) Coordinates


type Item msg
    = Item ( Int -> Coloring -> Svg msg, String, msg )
    | Divider
