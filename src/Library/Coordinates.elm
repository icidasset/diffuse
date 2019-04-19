module Coordinates exposing (Coordinates, fromTuple)

-- 🌳


type alias Coordinates =
    { x : Float, y : Float }



-- 🔱


fromTuple : ( Float, Float ) -> Coordinates
fromTuple ( x, y ) =
    { x = x
    , y = y
    }
