module Tracks.Collection.Internal.Identify exposing (identify)

import Dict
import List.Extra as List
import Time.Ext as Time
import Tracks exposing (..)



-- 🔱


identify : Parcel -> Parcel
identify ( deps, collection ) =
    let
        ( favouritesDictionary, simplifiedFavourites ) =
            List.foldr
                (\fav ( dict, acc ) ->
                    let
                        simpl =
                            case fav.artist of
                                Just artist ->
                                    String.toLower artist ++ String.toLower fav.title

                                Nothing ->
                                    String.toLower fav.title
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
    if List.member track.sourceId enabledSourceIds then
        partTwo favourites track

    else
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

        { filename, parentDirectory } =
            pathParts track

        identifiedTrack =
            ( { isFavourite = isFav
              , isMissing = False

              --
              , filename = filename
              , group = Nothing
              , indexInList = 0
              , indexInPlaylist = Nothing
              , parentDirectory = parentDirectory
              }
            , track
            )
    in
    if isFav then
        --
        -- A favourite
        --
        ( identifiedTrack :: acc
        , remainingFavourites
            |> List.findIndex isFavourite_
            |> Maybe.map (\idx -> List.removeAt idx remainingFavourites)
            |> Maybe.withDefault remainingFavourites
        )

    else
        --
        -- Not a favourite
        --
        ( identifiedTrack :: acc
        , remainingFavourites
        )



-- FAVOURITES


isFavourite : Track -> String -> Bool
isFavourite track =
    -- This needs to match the `simplifiedFavourites` format from above
    (==)
        (case track.tags.artist of
            Just artist ->
                String.toLower artist ++ String.toLower track.tags.title

            Nothing ->
                String.toLower track.tags.title
        )


makeMissingFavouriteTrack : Favourite -> IdentifiedTrack
makeMissingFavouriteTrack fav =
    let
        tags =
            { disc = 1
            , nr = 0
            , artist = fav.artist
            , title = fav.title
            , album = Nothing
            , genre = Nothing
            , picture = Nothing
            , year = Nothing
            }
    in
    ( { isFavourite = True
      , isMissing = True

      --
      , filename = ""
      , group = Nothing
      , indexInList = 0
      , indexInPlaylist = Nothing
      , parentDirectory = ""
      }
    , { tags = tags
      , id = missingId
      , insertedAt = Time.default
      , path = missingId
      , sourceId = missingId
      }
    )
