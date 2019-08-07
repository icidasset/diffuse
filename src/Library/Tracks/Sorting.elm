module Tracks.Sorting exposing (sort)

import Tracks exposing (..)



-- 🔱


sort : SortBy -> SortDirection -> List IdentifiedTrack -> List IdentifiedTrack
sort property direction list =
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
    list
        |> List.sortWith sortFn
        |> dirFn



-- BY


sortByAlbum : IdentifiedTrack -> IdentifiedTrack -> Order
sortByAlbum ( _, a ) ( _, b ) =
    EQ
        |> andThenCompare album a b
        |> andThenCompare disc a b
        |> andThenCompare nr a b
        |> andThenCompare artist a b
        |> andThenCompare title a b


sortByArtist : IdentifiedTrack -> IdentifiedTrack -> Order
sortByArtist ( _, a ) ( _, b ) =
    EQ
        |> andThenCompare artist a b
        |> andThenCompare album a b
        |> andThenCompare disc a b
        |> andThenCompare nr a b
        |> andThenCompare title a b


sortByTitle : IdentifiedTrack -> IdentifiedTrack -> Order
sortByTitle ( _, a ) ( _, b ) =
    EQ
        |> andThenCompare title a b
        |> andThenCompare artist a b
        |> andThenCompare album a b


sortByPlaylistIndex : IdentifiedTrack -> IdentifiedTrack -> Order
sortByPlaylistIndex ( a, _ ) ( b, _ ) =
    andThenCompare (.indexInPlaylist >> Maybe.withDefault 0) a b EQ



-- TAGS


album : Track -> String
album =
    .tags >> .album >> low


artist : Track -> String
artist =
    .tags >> .artist >> low


title : Track -> String
title =
    .tags >> .title >> low


disc : Track -> Int
disc =
    .tags >> .disc


nr : Track -> Int
nr =
    .tags >> .nr



-- COMMON


andThenCompare : (ctx -> comparable) -> ctx -> ctx -> Order -> Order
andThenCompare fn a b order =
    if order == EQ then
        compare (fn a) (fn b)

    else
        order


low : String -> String
low =
    String.toLower
