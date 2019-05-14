module Tracks.Favourites exposing (match, simplified, toggleInFavouritesList, toggleInTracksList)

import List.Extra as List
import Tracks exposing (Favourite, IdentifiedTrack, Track)



-- 🔱


match : Favourite -> Favourite -> Bool
match a b =
    let
        ( aa, at ) =
            ( String.toLower a.artist
            , String.toLower a.title
            )

        ( ba, bt ) =
            ( String.toLower b.artist
            , String.toLower b.title
            )
    in
    aa == ba && at == bt


simplified : Favourite -> String
simplified fav =
    String.toLower fav.artist ++ String.toLower fav.title


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
    case i.isFavourite of
        True ->
            -- Remove from list
            List.filterNot
                (match favourite)
                favourites

        False ->
            -- Add to list
            List.append
                favourites
                [ favourite ]



-- ⚗️


lowercaseArtist : Track -> String
lowercaseArtist =
    .tags >> .artist >> String.toLower


lowercaseTitle : Track -> String
lowercaseTitle =
    .tags >> .title >> String.toLower
