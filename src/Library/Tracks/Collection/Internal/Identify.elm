module Tracks.Collection.Internal.Identify exposing (identify)

import Dict
import List.Extra as List
import Time.Ext as Time
import Tracks exposing (..)
import Tracks.Favourites as Favourites



-- 🔱


identify : Parcel -> Parcel
identify ( deps, collection ) =
    let
        ( favouritesDictionary, simplifiedFavourites ) =
            List.foldr
                (\fav ( dict, acc ) ->
                    let
                        simpl =
                            Favourites.simplified fav
                    in
                    ( Dict.insert simpl fav dict
                    , simpl :: acc
                    )
                )
                ( Dict.empty, [] )
                deps.favourites

        ( identifiedUnsorted, missingFavouritesSimplified ) =
            List.foldl
                (identifyTrack
                    deps.enabledSourceIds
                    simplifiedFavourites
                )
                ( [], simplifiedFavourites )
                collection.untouched

        missingFavourites =
            List.foldr
                (\simpl acc ->
                    case Dict.get simpl favouritesDictionary of
                        Just fav ->
                            fav :: acc

                        Nothing ->
                            acc
                )
                []
                missingFavouritesSimplified
    in
    identifiedUnsorted
        |> List.append (List.map makeMissingFavouriteTrack missingFavourites)
        |> (\x -> { collection | identified = x })
        |> (\x -> ( deps, x ))



-- IDENTIFY


identifyTrack :
    List String
    -> List String
    -> Track
    -> ( List IdentifiedTrack, List String )
    -> ( List IdentifiedTrack, List String )
identifyTrack enabledSourceIds favourites track =
    case List.member track.sourceId enabledSourceIds of
        True ->
            partTwo favourites track

        False ->
            identity


partTwo :
    List String
    -> Track
    -> ( List IdentifiedTrack, List String )
    -> ( List IdentifiedTrack, List String )
partTwo favourites track ( acc, remainingFavourites ) =
    let
        isFavourite_ =
            isFavourite track

        isFav =
            List.any isFavourite_ favourites

        identifiedTrack =
            ( { isFavourite = isFav
              , isMissing = False

              --
              , group = Nothing
              , indexInList = 0
              , indexInPlaylist = Nothing
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


isFavourite : Track -> String -> Bool
isFavourite track =
    (==) (String.toLower track.tags.artist ++ String.toLower track.tags.title)


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
    ( { isFavourite = True
      , isMissing = True

      --
      , group = Nothing
      , indexInList = 0
      , indexInPlaylist = Nothing
      }
    , { tags = tags
      , id = missingId
      , insertedAt = Time.default
      , path = missingId
      , sourceId = missingId
      }
    )
