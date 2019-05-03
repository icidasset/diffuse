module ContextMenu exposing (ContextMenu(..), ContextMenuItems)

import Color exposing (Color)
import Coordinates exposing (Coordinates)
import Material.Icons exposing (Coloring(..))
import Svg exposing (Svg)



-- 🌳


type ContextMenu msg
    = ContextMenu (ContextMenuItems msg) Coordinates


type alias ContextMenuItems msg =
    List ( Int -> Coloring -> Svg msg, String, msg )
