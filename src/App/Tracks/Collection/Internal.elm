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
import Maybe.Extra as Maybe
import Playlists.Types exposing (PlaylistTrack)
import Tracks.Favourites as Favourites
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
    let
        enabledOnly =
            List.filter
                (\t -> List.member t.sourceId model.enabledSourceIds)
                collection.untouched

        playlistTracks =
            model.selectedPlaylist
                |> Maybe.map .tracks
                |> Maybe.withDefault []

        ( identifiedUnsorted, missingFavourites, missingPlaylistTracks ) =
            List.foldr
                (identifier model.favourites model.activeTrackId)
                ( [], model.favourites, playlistTracks )
                enabledOnly
    in
        identifiedUnsorted
            |> List.append (List.map makeMissingFavouriteTrack missingFavourites)
            |> List.append (List.map makeMissingPlaylistTrack missingPlaylistTracks)
            |> Sorting.sort model.sortBy model.sortDirection
            |> (\x -> { collection | identified = x })
            |> (\x -> (,) model x)


identifier :
    List Favourite
    -> Maybe TrackId
    -> Track
    -> ( List IdentifiedTrack, List Favourite, List PlaylistTrack )
    -> ( List IdentifiedTrack, List Favourite, List PlaylistTrack )
identifier favourites activeTrackId track ( acc, missingFavourites, missingPlaylistTracks ) =
    let
        lalbum =
            String.toLower track.tags.album

        lartist =
            String.toLower track.tags.artist

        ltitle =
            String.toLower track.tags.title

        isNowPlaying =
            Just track.id == activeTrackId

        favouriteMatcher =
            Favourites.matcher lartist ltitle

        isFavourite =
            List.any favouriteMatcher favourites

        identifiedTrack =
            (,)
                { isFavourite = False
                , isMissing = False
                , isNowPlaying = isNowPlaying
                }
                track

        missingPlaylistTracks_ =
            List.filterNot
                (\{ album, artist, title } ->
                    (lalbum == String.toLower album)
                        && (lartist == String.toLower artist)
                        && (ltitle == String.toLower title)
                )
                missingPlaylistTracks
    in
        case isFavourite of
            -- A favourite
            --
            True ->
                ( identifiedTrack
                    |> Tuple.mapFirst (\i -> { i | isFavourite = True })
                    |> (flip (::)) acc
                  --
                , if isFavourite then
                    List.filterNot favouriteMatcher missingFavourites
                  else
                    missingFavourites
                  --
                , missingPlaylistTracks_
                )

            -- Not a favourite
            --
            False ->
                ( identifiedTrack :: acc
                , missingFavourites
                , missingPlaylistTracks_
                )


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
            { isFavourite = True, isMissing = True, isNowPlaying = False }
            { tags = tags, id = missingId, path = missingId, sourceId = missingId }


makeMissingPlaylistTrack : PlaylistTrack -> IdentifiedTrack
makeMissingPlaylistTrack playlistTrack =
    let
        tags =
            { disc = 1
            , nr = 0
            , artist = playlistTrack.artist
            , title = playlistTrack.title
            , album = playlistTrack.album
            , genre = Nothing
            , picture = Nothing
            , year = Nothing
            }
    in
        (,)
            { isFavourite = False, isMissing = True, isNowPlaying = False }
            { tags = tags, id = missingId, path = missingId, sourceId = missingId }



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

        filters =
            [ --
              -- Favourites / Missing
              if model.favouritesOnly then
                Tuple.first >> .isFavourite >> (==) True
              else if Maybe.isJust model.selectedPlaylist then
                always True
              else
                Tuple.first >> .isMissing >> (==) False

            --
            -- Playlists
            , case model.selectedPlaylist of
                Just playlist ->
                    case playlist.autoGenerated of
                        True ->
                            \( _, t ) ->
                                t.path
                                    |> String.split "/"
                                    |> List.head
                                    |> Maybe.withDefault ""
                                    |> (==) playlist.name

                        False ->
                            \( _, t ) ->
                                List.member
                                    { album = t.tags.album
                                    , artist = t.tags.artist
                                    , title = t.tags.title
                                    }
                                    playlist.tracks

                Nothing ->
                    always True
            ]

        theFilter =
            \x ->
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
        (,)
            model
            { collection | harvested = List.filter theFilter harvested }


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
