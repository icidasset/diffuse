module Tracks.Favourites exposing (toggleInModel)

import Firebase.Data
import List.Extra as List
import Tracks.Collection exposing (makeParcel, reharvest, remap, set)
import Tracks.Encoding
import Tracks.Types exposing (..)
import Types as TopLevel
import Utils exposing (addCmd)


-- 🍯


toggleInModel : Model -> IdentifiedTrack -> ( Model, Cmd TopLevel.Msg )
toggleInModel model ( i, t ) =
    let
        newFavourites =
            toggleInList model.favourites ( i, t )

        storeFavourites =
            newFavourites
                |> List.map Tracks.Encoding.encodeFavourite
                |> Firebase.Data.storeFavourites

        effect =
            if model.favouritesOnly then
                remap (toggleInCollection t) >> reharvest
            else
                remap (toggleInCollection t)
    in
        { model | favourites = newFavourites }
            |> makeParcel
            |> effect
            |> set
            |> addCmd storeFavourites



-- ❌


toggleInCollection : Track -> List IdentifiedTrack -> List IdentifiedTrack
toggleInCollection track collection =
    let
        indexer =
            Tuple.second >> .id >> (==) track.id

        updater =
            Tuple.mapFirst (\i -> { i | isFavourite = not i.isFavourite })
    in
        collection
            |> List.findIndex indexer
            |> Maybe.andThen (\idx -> List.updateAt idx updater collection)
            |> Maybe.withDefault collection


toggleInList : List Favourite -> IdentifiedTrack -> List Favourite
toggleInList favourites ( i, t ) =
    let
        artist =
            String.toLower t.tags.artist

        title =
            String.toLower t.tags.title
    in
        case i.isFavourite of
            True ->
                List.filter
                    (\f -> not (f.artist == artist && f.title == title))
                    favourites

            False ->
                List.append
                    favourites
                    [ { artist = artist
                      , title = title
                      }
                    ]
