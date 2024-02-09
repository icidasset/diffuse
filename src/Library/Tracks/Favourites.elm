module Tracks.Favourites exposing (completeFavouritesList, completeTracksList, match, removeFromFavouritesList, removeFromTracksList, toggleInFavouritesList, toggleInTracksList)

import List.Extra as List
import Maybe.Extra as Maybe
import Tracks exposing (Favourite, IdentifiedTrack, Track, fallbackArtist)



-- 🔱


completeFavouritesList : List IdentifiedTrack -> List Favourite -> List Favourite
completeFavouritesList tracks favourites =
    List.append
        favourites
        (List.filterMap
            (\( i, t ) ->
                if not i.isFavourite then
                    Just
                        { artist = t.tags.artist
                        , title = t.tags.title
                        }

                else
                    Nothing
            )
            tracks
        )


completeTracksList : List IdentifiedTrack -> List IdentifiedTrack -> List IdentifiedTrack
completeTracksList tracksToMakeFavourite tracks =
    let
        favs =
            List.filterMap
                (\( i, t ) ->
                    if not i.isFavourite then
                        Just ( lowercaseArtist t, lowercaseTitle t )

                    else
                        Nothing
                )
                tracksToMakeFavourite
    in
    List.map
        (\( i, t ) ->
            let
                ( la, lt ) =
                    ( lowercaseArtist t, lowercaseTitle t )
            in
            List.foldr
                (\( lartist, ltitle ) ( ai, at ) ->
                    if la == lartist && lt == ltitle && not ai.isFavourite then
                        ( { ai | isFavourite = True }, at )

                    else
                        ( ai, at )
                )
                ( i, t )
                favs
        )
        tracks


match : Favourite -> Favourite -> Bool
match a b =
    let
        ( aa, at ) =
            ( Maybe.unwrap "" String.toLower a.artist
            , String.toLower a.title
            )

        ( ba, bt ) =
            ( Maybe.unwrap "" String.toLower b.artist
            , String.toLower b.title
            )
    in
    aa == ba && at == bt


removeFromFavouritesList : List IdentifiedTrack -> List Favourite -> List Favourite
removeFromFavouritesList tracks favourites =
    List.foldr
        (\( i, t ) acc ->
            if i.isFavourite then
                List.filterNot
                    (match { artist = t.tags.artist, title = t.tags.title })
                    acc

            else
                acc
        )
        favourites
        tracks


removeFromTracksList : List IdentifiedTrack -> List IdentifiedTrack -> List IdentifiedTrack
removeFromTracksList tracksToRemoveFromFavs tracks =
    let
        unfavs =
            List.filterMap
                (\( i, t ) ->
                    if i.isFavourite then
                        Just ( lowercaseArtist t, lowercaseTitle t )

                    else
                        Nothing
                )
                tracksToRemoveFromFavs
    in
    List.map
        (\( i, t ) ->
            let
                ( la, lt ) =
                    ( lowercaseArtist t, lowercaseTitle t )
            in
            List.foldr
                (\( lartist, ltitle ) ( ai, at ) ->
                    if la == lartist && lt == ltitle && ai.isFavourite then
                        ( { ai | isFavourite = False }, at )

                    else
                        ( ai, at )
                )
                ( i, t )
                unfavs
        )
        tracks


toggleInTracksList : Track -> List IdentifiedTrack -> List IdentifiedTrack
toggleInTracksList track =
    let
        lartist =
            lowercaseArtist track

        ltitle =
            lowercaseTitle track
    in
    List.map
        (\( i, t ) ->
            if lowercaseArtist t == lartist && lowercaseTitle t == ltitle then
                ( { i | isFavourite = not i.isFavourite }, t )

            else
                ( i, t )
        )


toggleInFavouritesList : IdentifiedTrack -> List Favourite -> List Favourite
toggleInFavouritesList ( i, t ) favourites =
    let
        favourite =
            { artist = t.tags.artist
            , title = t.tags.title
            }
    in
    if i.isFavourite then
        -- Remove from list
        List.filterNot
            (match favourite)
            favourites

    else
        -- Add to list
        List.append
            favourites
            [ favourite ]



-- ⚗️


lowercaseArtist : Track -> String
lowercaseArtist =
    -- NOTE: Not entirely sure this fallback is correct
    .tags >> .artist >> Maybe.unwrap fallbackArtist String.toLower


lowercaseTitle : Track -> String
lowercaseTitle =
    .tags >> .title >> String.toLower
