module Tracks.Collection.Internal
    exposing
        ( build
        , buildf
        , partial
        , initialize
        , identify
        , harvest
        , expose
        )

import List.Extra as List
import Tracks.Sorting as Sorting
import Tracks.Types exposing (..)


build : List Track -> Parcel -> Parcel
build tracks =
    initialize tracks >> identify >> harvest >> expose


buildf : Parcel -> List Track -> Parcel
buildf =
    flip build


partial : Int
partial =
    50



-- Initialize


initialize : List Track -> Parcel -> Parcel
initialize tracks ( model, collection ) =
    (,) model { collection | untouched = tracks }



-- Identifying


identify : Parcel -> Parcel
identify ( model, collection ) =
    collection.untouched
        |> List.foldl identifier ( [], model.favourites )
        |> Tuple.first
        |> Sorting.sort model.sortBy model.sortDirection
        |> (\x -> { collection | identified = x })
        |> (\x -> (,) model x)


identifier :
    Track
    -> ( List IdentifiedTrack, List Favourite )
    -> ( List IdentifiedTrack, List Favourite )
identifier track ( acc, favourites ) =
    let
        artist =
            String.toLower track.tags.artist

        title =
            String.toLower track.tags.title

        idx =
            List.findIndex
                (\f -> f.artist == artist && f.title == title)
                favourites
    in
        case idx of
            Just i ->
                ( acc ++ [ ( { isFavourite = True }, track ) ]
                , List.removeAt i favourites
                )

            Nothing ->
                ( acc ++ [ ( { isFavourite = False }, track ) ]
                , favourites
                )



-- Harvesting


harvest : Parcel -> Parcel
harvest ( model, collection ) =
    let
        harvested =
            case model.searchResults of
                Just [] ->
                    []

                Just trackIds ->
                    collection.identified
                        |> List.foldl harvester ( [], trackIds )
                        |> Tuple.first

                Nothing ->
                    collection.identified

        filtered =
            if model.favouritesOnly then
                List.filter (\( i, t ) -> i.isFavourite == True) harvested
            else
                harvested
    in
        (,)
            model
            { collection | harvested = filtered }


harvester :
    IdentifiedTrack
    -> ( List IdentifiedTrack, List TrackId )
    -> ( List IdentifiedTrack, List TrackId )
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



-- Exposing


expose : Parcel -> Parcel
expose ( model, collection ) =
    (,)
        model
        { collection | exposed = List.take (model.exposedStep * partial) collection.harvested }
