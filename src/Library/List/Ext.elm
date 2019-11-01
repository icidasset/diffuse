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

    >>> move { from = 0, to = 2 } [1, 2, 3]
    [2, 1, 3]

    >>> move { from = 2, to = 0 } [1, 2, 3]
    [3, 1, 2]

-}
move : { from : Int, to : Int } -> List a -> List a
move opts list =
    let
        from =
            opts.from

        to =
            if opts.to > from then
                opts.to - 1

            else
                opts.to

        maybeItemToMove =
            List.getAt from list
    in
    list
        |> List.removeAt from
        |> List.indexedFoldr
            (\idx existingItem acc ->
                if idx == to then
                    case maybeItemToMove of
                        Just itemToMove ->
                            List.append [ itemToMove, existingItem ] acc

                        Nothing ->
                            existingItem :: acc

                else
                    existingItem :: acc
            )
            []


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
