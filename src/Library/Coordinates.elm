module Coordinates exposing (..)

-- 🌳


type alias Coordinates =
    { x : Float, y : Float }


type alias Viewport =
    { height : Float
    , width : Float
    }



-- 🔱


fromTuple : ( Float, Float ) -> Coordinates
fromTuple ( x, y ) =
    { x = x
    , y = y
    }


toTuple : Coordinates -> ( Float, Float )
toTuple { x, y } =
    ( x, y )
