module String.Path exposing (..)

import List.Extra as List



-- ⛩


sep : String
sep =
    "/"



-- 🔱


addSuffix : String -> String
addSuffix path =
    case path of
        "" ->
            ""

        p ->
            p ++ sep


{-| Drop an amount of path segments from the right side.

    >>> dropRight 5 "a/b/c/d/e/f"
    "a"

-}
dropRight : Int -> String -> String
dropRight int path =
    path
        |> String.split sep
        |> (\l -> List.take (List.length l - int) l)
        |> String.join sep


file : String -> String
file path =
    path
        |> String.split sep
        |> List.last
        |> Maybe.withDefault path
