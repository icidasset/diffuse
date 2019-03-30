module Tracks.Collection.Internal.Harvest exposing (harvest)

import List.Extra as List
import Maybe.Extra as Maybe
import Tracks exposing (..)



-- 🍯


harvest : Parcel -> Parcel
harvest ( deps, collection ) =
    let
        harvested =
            case deps.searchResults of
                Just [] ->
                    []

                Just trackIds ->
                    collection.arranged
                        |> List.foldl harvester ( [], trackIds )
                        |> Tuple.first

                Nothing ->
                    collection.arranged

        filters =
            [ -- Favourites / Missing
              -----------------------
              if deps.favouritesOnly then
                Tuple.first >> .isFavourite >> (==) True

              else
                Tuple.first >> .isMissing >> (==) False
            ]

        theFilter x =
            List.foldl
                (\filter bool ->
                    if bool == True then
                        filter x

                    else
                        bool
                )
                True
                filters
    in
    harvested
        |> List.filter theFilter
        |> (if deps.hideDuplicates then
                List.foldr
                    (\( i, t ) ( seen, acc ) ->
                        let
                            s =
                                String.toLower (t.tags.artist ++ "/" ++ t.tags.title)
                        in
                        if List.member s seen then
                            ( seen, acc )

                        else
                            ( s :: seen, ( i, t ) :: acc )
                    )
                    ( [], [] )
                    >> Tuple.second

            else
                identity
           )
        |> List.indexedMap (\idx tup -> Tuple.mapFirst (\i -> { i | indexInList = idx }) tup)
        |> (\h -> { collection | harvested = h })
        |> (\c -> ( deps, c ))


harvester :
    IdentifiedTrack
    -> ( List IdentifiedTrack, List String )
    -> ( List IdentifiedTrack, List String )
harvester ( i, t ) ( acc, trackIds ) =
    case List.findIndex ((==) t.id) trackIds of
        Just idx ->
            ( acc ++ [ ( i, t ) ]
            , List.removeAt idx trackIds
            )

        Nothing ->
            ( acc
            , trackIds
            )
