module ContextMenu exposing (ContextMenu(..), ContextMenuItems)

import Color exposing (Color)
import Coordinates exposing (Coordinates)
import Svg exposing (Svg)



-- 🌳


type ContextMenu msg
    = ContextMenu (ContextMenuItems msg) Coordinates


type alias ContextMenuItems msg =
    List ( Color -> Int -> Svg msg, String, msg )
