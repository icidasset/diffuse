module Tracks.Favourites exposing (..)

import List.Extra as List
import Tracks.Types exposing (..)


matcher : String -> String -> Favourite -> Bool
matcher lowerArtist lowerTitle fav =
    let
        lowerFavArtist =
            String.toLower fav.artist

        lowerFavTitle =
            String.toLower fav.title
    in
        lowerFavArtist == lowerArtist && lowerFavTitle == lowerTitle


toggleInCollection : Track -> List IdentifiedTrack -> List IdentifiedTrack
toggleInCollection track collection =
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
            collection


toggleInList : List Favourite -> IdentifiedTrack -> List Favourite
toggleInList favourites ( i, t ) =
    case i.isFavourite of
        True ->
            -- Remove from list
            List.filterNot
                (matcher (lowercaseArtist t) (lowercaseTitle t))
                favourites

        False ->
            -- Add to list
            List.append
                favourites
                [ { artist = t.tags.artist
                  , title = t.tags.title
                  }
                ]



-- Utils


lowercaseArtist : Track -> String
lowercaseArtist =
    .tags >> .artist >> String.toLower


lowercaseTitle : Track -> String
lowercaseTitle =
    .tags >> .title >> String.toLower
