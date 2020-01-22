module ContextMenu exposing (ContextMenu(..), Item(..), ItemProperties, anyItem, coordinates, justAnItem)

import Coordinates exposing (Coordinates)
import Material.Icons.Types exposing (Coloring(..))
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



-- 🔱


anyItem : (ItemProperties msg -> Bool) -> ContextMenu msg -> Bool
anyItem fn (ContextMenu items _) =
    List.any
        (\item ->
            case item of
                Item props ->
                    fn props

                Divider ->
                    False
        )
        items


coordinates : ContextMenu msg -> Coordinates
coordinates (ContextMenu _ c) =
    c


justAnItem : ItemProperties msg -> Maybe (Item msg)
justAnItem =
    Just << Item
