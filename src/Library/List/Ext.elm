module List.Ext exposing (addTo, move)

import List.Extra as List


{-| Flipped version of (::).
-}
addTo : List a -> a -> List a
addTo list item =
    item :: list


{-| Move an item "from" an index "to" another index.
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
