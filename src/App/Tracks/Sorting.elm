module Tracks.Sorting exposing (sort)

import Char
import Tracks.Types exposing (..)


sort : SortBy -> SortDirection -> List IdentifiedTrack -> List IdentifiedTrack
sort property direction =
    let
        sortFn =
            case property of
                Album ->
                    sortByAlbum

                Artist ->
                    sortByArtist

                PlaylistIndex ->
                    sortByPlaylistIndex

                Title ->
                    sortByTitle

        dirFn =
            if direction == Desc then
                List.reverse
            else
                identity
    in
        List.sortWith sortFn >> dirFn



-- {by} Album


sortByAlbum : IdentifiedTrack -> IdentifiedTrack -> Order
sortByAlbum ( _, a ) ( _, b ) =
    EQ
        |> andThenCompare (.tags >> .album >> low) a b
        |> andThenCompare (.tags >> .disc) a b
        |> andThenCompare (.tags >> .nr) a b
        |> andThenCompare (.tags >> .artist >> low) a b
        |> andThenCompare (.tags >> .title >> low) a b



-- {by} Artist


sortByArtist : IdentifiedTrack -> IdentifiedTrack -> Order
sortByArtist ( _, a ) ( _, b ) =
    EQ
        |> andThenCompare (.tags >> .artist >> low) a b
        |> andThenCompare (.tags >> .album >> low) a b
        |> andThenCompare (.tags >> .disc) a b
        |> andThenCompare (.tags >> .nr) a b
        |> andThenCompare (.tags >> .title >> low) a b



-- {by} Title


sortByTitle : IdentifiedTrack -> IdentifiedTrack -> Order
sortByTitle ( _, a ) ( _, b ) =
    EQ
        |> andThenCompare (.tags >> .title >> low) a b
        |> andThenCompare (.tags >> .artist >> low) a b
        |> andThenCompare (.tags >> .album >> low) a b



-- {by} Playlist index


sortByPlaylistIndex : IdentifiedTrack -> IdentifiedTrack -> Order
sortByPlaylistIndex ( a, _ ) ( b, _ ) =
    andThenCompare (.indexInPlaylist >> Maybe.withDefault 0) a b EQ



-- Utils


andThenCompare : (ctx -> comparable) -> ctx -> ctx -> Order -> Order
andThenCompare fn a b order =
    if order == EQ then
        compare (fn a) (fn b)
    else
        order


low : String -> String
low =
    String.toLower
