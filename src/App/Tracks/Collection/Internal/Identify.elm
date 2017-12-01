module Tracks.Collection.Internal.Identify exposing (identify)

import List.Extra as List
import Tracks.Favourites as Favourites
import Tracks.Types exposing (..)


-- 🍯


identify : Parcel -> Parcel
identify ( model, collection ) =
    let
        ( identifiedUnsorted, missingFavourites ) =
            List.foldl
                (stepOne
                    model.enabledSourceIds
                    model.favourites
                    model.activeIdentifiedTrack
                )
                ( [], model.favourites )
                collection.untouched
    in
        identifiedUnsorted
            |> List.append (List.map makeMissingFavouriteTrack missingFavourites)
            |> (\x -> { collection | identified = x })
            |> (\x -> (,) model x)



-- Identifier


stepOne :
    List SourceId
    -> List Favourite
    -> Maybe IdentifiedTrack
    -> Track
    -> ( List IdentifiedTrack, List Favourite )
    -> ( List IdentifiedTrack, List Favourite )
stepOne enabledSourceIds favourites nowPlaying track tuple =
    case List.member track.sourceId enabledSourceIds of
        True ->
            stepTwo favourites nowPlaying track tuple

        False ->
            tuple


stepTwo :
    List Favourite
    -> Maybe IdentifiedTrack
    -> Track
    -> ( List IdentifiedTrack, List Favourite )
    -> ( List IdentifiedTrack, List Favourite )
stepTwo favourites nowPlaying track ( acc, remainingFavourites ) =
    let
        isNP =
            nowPlaying
                |> Maybe.map (Tuple.second >> .id >> (==) track.id)
                |> Maybe.withDefault False

        isFav =
            List.any (isFavourite track) favourites

        identifiedTrack =
            (,)
                { indexInPlaylist = Nothing
                , isFavourite = isFav
                , isMissing = False
                , isNowPlaying = isNP
                }
                track
    in
        case isFav of
            --
            -- A favourite
            --
            True ->
                ( identifiedTrack :: acc
                , remainingFavourites
                    |> List.findIndex (isFavourite track)
                    |> Maybe.map (\idx -> List.removeAt idx remainingFavourites)
                    |> Maybe.withDefault remainingFavourites
                )

            --
            -- Not a favourite
            --
            False ->
                ( identifiedTrack :: acc
                , remainingFavourites
                )



-- Favourites


isFavourite : Track -> (Favourite -> Bool)
isFavourite track =
    let
        lartist =
            String.toLower track.tags.artist

        ltitle =
            String.toLower track.tags.title
    in
        Favourites.matcher lartist ltitle


makeMissingFavouriteTrack : Favourite -> IdentifiedTrack
makeMissingFavouriteTrack fav =
    let
        tags =
            { disc = 1
            , nr = 0
            , artist = fav.artist
            , title = fav.title
            , album = "<missing>"
            , genre = Nothing
            , picture = Nothing
            , year = Nothing
            }
    in
        (,)
            { indexInPlaylist = Nothing
            , isFavourite = True
            , isMissing = True
            , isNowPlaying = False
            }
            { tags = tags
            , id = missingId
            , path = missingId
            , sourceId = missingId
            }
