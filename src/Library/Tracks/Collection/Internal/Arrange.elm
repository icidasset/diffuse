module Tracks.Collection.Internal.Arrange exposing (arrange)

import Tracks exposing (..)
import Tracks.Sorting as Sorting



-- 🍯


arrange : Parcel -> Parcel
arrange ( deps, collection ) =
    collection.identified
        |> Sorting.sort deps.sortBy deps.sortDirection
        |> (\x -> { collection | arranged = x })
        |> (\x -> ( deps, x ))
