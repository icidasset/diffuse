module Tracks.Sorting exposing (..)

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

                Title ->
                    sortByTitle

        dirFn =
            if direction == Desc then
                List.reverse
            else
                identity
    in
        List.sortBy sortFn >> dirFn


sortByAlbum : IdentifiedTrack -> String
sortByAlbum twi =
    let
        t =
            Tuple.second twi
    in
        t.tags.title
            |> String.append (toString t.tags.nr)
            |> String.append t.tags.artist
            |> String.append t.tags.album
            |> String.toLower


sortByArtist : IdentifiedTrack -> String
sortByArtist twi =
    let
        t =
            Tuple.second twi
    in
        t.tags.title
            |> String.append (toString t.tags.nr)
            |> String.append t.tags.album
            |> String.append t.tags.artist
            |> String.toLower


sortByTitle : IdentifiedTrack -> String
sortByTitle twi =
    let
        t =
            Tuple.second twi
    in
        t.tags.album
            |> String.append t.tags.artist
            |> String.append t.tags.title
            |> String.toLower
