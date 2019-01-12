module Tracks.Collection.Internal.Identify exposing (identify)

import List.Extra as List
import Tracks exposing (..)
import Tracks.Favourites as Favourites



-- 🔱


identify : Parcel -> Parcel
identify ( deps, collection ) =
    let
        ( identifiedUnsorted, missingFavourites ) =
            List.foldl
                (identifyTrack
                    deps.enabledSourceIds
                    deps.favourites
                    deps.nowPlaying
                )
                ( [], deps.favourites )
                collection.untouched
    in
    identifiedUnsorted
        |> List.append (List.map makeMissingFavouriteTrack missingFavourites)
        |> (\x -> { collection | identified = x })
        |> (\x -> ( deps, x ))



-- IDENTIFY


identifyTrack :
    List String
    -> List Favourite
    -> Maybe IdentifiedTrack
    -> Track
    -> ( List IdentifiedTrack, List Favourite )
    -> ( List IdentifiedTrack, List Favourite )
identifyTrack enabledSourceIds favourites nowPlaying track =
    case List.member track.sourceId enabledSourceIds of
        True ->
            partTwo favourites nowPlaying track

        False ->
            identity


partTwo :
    List Favourite
    -> Maybe IdentifiedTrack
    -> Track
    -> ( List IdentifiedTrack, List Favourite )
    -> ( List IdentifiedTrack, List Favourite )
partTwo favourites nowPlaying track ( acc, remainingFavourites ) =
    let
        isNP =
            nowPlaying
                |> Maybe.map (Tuple.second >> .id >> (==) track.id)
                |> Maybe.withDefault False

        isFavourite_ =
            isFavourite track

        isFav =
            List.any isFavourite_ favourites

        identifiedTrack =
            ( { indexInList = 0
              , indexInPlaylist = Nothing
              , isFavourite = isFav
              , isMissing = False
              , isNowPlaying = isNP
              , isSelected = False
              }
            , track
            )
    in
    case isFav of
        --
        -- A favourite
        --
        True ->
            ( identifiedTrack :: acc
            , remainingFavourites
                |> List.findIndex isFavourite_
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



-- FAVOURITES


isFavourite : Track -> (Favourite -> Bool)
isFavourite track =
    Favourites.match
        { artist = track.tags.artist
        , title = track.tags.title
        }


makeMissingFavouriteTrack : Favourite -> IdentifiedTrack
makeMissingFavouriteTrack fav =
    let
        tags =
            { disc = 1
            , nr = 0
            , artist = fav.artist
            , title = fav.title
            , album = missingId
            , genre = Nothing
            , picture = Nothing
            , year = Nothing
            }
    in
    ( { indexInList = 0
      , indexInPlaylist = Nothing
      , isFavourite = True
      , isMissing = True
      , isNowPlaying = False
      , isSelected = False
      }
    , { tags = tags
      , id = missingId
      , path = missingId
      , sourceId = missingId
      }
    )
