module List.Ext exposing (..)

import List.Extra as List


{-| Flipped version of `append`.

    >>> add [2, 3] [1]
    [1, 2, 3]

-}
add : List a -> List a -> List a
add a b =
    List.append b a


{-| Flipped version of (::).

    >>> addTo [2, 3] 1
    [1, 2, 3]

-}
addTo : List a -> a -> List a
addTo list item =
    item :: list


{-| Move an item "from" an index "to" another index.
Putting the item in front of the `to` index.

    >>> move { from = 0, to = 2, amount = 1 } [1, 2, 3]
    [2, 1, 3]

    >>> move { from = 2, to = 0, amount = 1 } [1, 2, 3]
    [3, 1, 2]

    >>> move { from = 2, to = 7, amount = 3 } [0, 1, 2, 3, 4, 5, 6, 7]
    [0, 1, 5, 6, 2, 3, 4, 7]

    >>> move { from = 2, to = 1, amount = 3 } [0, 1, 2, 3, 4, 5, 6, 7]
    [0, 2, 3, 4, 1, 5, 6, 7]

-}
move : { amount : Int, from : Int, to : Int } -> List a -> List a
move { from, to, amount } list =
    []
        ++ (list |> List.take (min from to))
        ++ (list |> List.take to |> List.drop (from + amount))
        ++ (list |> List.drop from |> List.take amount)
        ++ (list |> List.take from |> List.drop to)
        ++ (list |> List.drop (max (from + amount) to))


pickIndexes : List Int -> List a -> List a
pickIndexes indexes items =
    List.foldr
        (\idx acc ->
            items
                |> List.getAt idx
                |> Maybe.map (addTo acc)
                |> Maybe.withDefault acc
        )
        []
        indexes


{-| Exclude a list from another list.

    >>> without [ 2 ] [ 1, 2, 3 ]
    [ 1, 3 ]

-}
without : List a -> List a -> List a
without exclude =
    List.filter (\c -> List.notMember c exclude)
