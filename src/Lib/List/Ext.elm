module List.Ext exposing (..)


addInFront : List a -> a -> List a
addInFront =
    flip (::)


{-| Move an item "from" an index "to" another index.
-}
move : { from : Int, to : Int } -> List a -> List a
move { from, to } list =
    if to <= from then
        []
            ++ (list |> List.take to)
            ++ (list |> List.drop from |> List.take 1)
            ++ (list |> List.take from |> List.drop to)
            ++ (list |> List.drop from |> List.drop 1)
    else
        []
            ++ (list |> List.take from)
            ++ (list |> List.take to |> List.drop from |> List.drop 1)
            ++ (list |> List.drop from |> List.take 1)
            ++ (list |> List.drop to)
