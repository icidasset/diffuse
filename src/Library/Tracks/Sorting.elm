module Tracks.Sorting exposing (sort)

import Maybe.Extra as Maybe
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
sortByAlbum ( x, a ) ( y, b ) =
    EQ
        |> andThenCompareBools isMissing x y
        |> andThenCompare album a b
        |> andThenCompare parentDir x y
        |> andThenCompare disc a b
        |> andThenCompare nr a b
        |> andThenCompare artist a b
        |> andThenCompare title a b


sortByArtist : IdentifiedTrack -> IdentifiedTrack -> Order
sortByArtist ( x, a ) ( y, b ) =
    EQ
        |> andThenCompareBools isMissing x y
        |> andThenCompare artist a b
        |> andThenCompare album a b
        |> andThenCompare parentDir x y
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
    .tags >> .album >> Maybe.unwrap fallbackAlbum low


artist : Track -> String
artist =
    .tags >> .artist >> Maybe.unwrap fallbackArtist low


title : Track -> String
title =
    .tags >> .title >> low


disc : Track -> Int
disc =
    .tags >> .disc


nr : Track -> Int
nr =
    .tags >> .nr


isMissing : Identifiers -> Bool
isMissing =
    .isMissing


parentDir : Identifiers -> String
parentDir =
    .parentDirectory >> low



-- COMMON


andThenCompare : (ctx -> comparable) -> ctx -> ctx -> Order -> Order
andThenCompare fn a b order =
    if order == EQ then
        compare (fn a) (fn b)

    else
        order


andThenCompareBools : (ctx -> Bool) -> ctx -> ctx -> Order -> Order
andThenCompareBools fn a b order =
    if order == EQ then
        let
            af =
                fn a

            bf =
                fn b
        in
        if af == bf then
            EQ

        else if af == False then
            GT

        else
            LT

    else
        order


low : String -> String
low =
    String.trim >> String.toLower
