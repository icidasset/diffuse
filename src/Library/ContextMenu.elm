module ContextMenu exposing (ContextMenu(..), Item(..), ItemProperties)

import Color exposing (Color)
import Coordinates exposing (Coordinates)
import Material.Icons exposing (Coloring(..))
import Svg exposing (Svg)



-- 🌳


type ContextMenu msg
    = ContextMenu (List (Item msg)) Coordinates


type Item msg
    = Item (ItemProperties msg)
    | Divider


type alias ItemProperties msg =
    { icon : Int -> Coloring -> Svg msg
    , label : String
    , msg : msg
    , active : Bool
    }
