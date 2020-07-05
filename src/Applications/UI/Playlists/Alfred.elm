module UI.Playlists.Alfred exposing (create, select)

import Alfred exposing (..)
import List.Extra as List
import Playlists exposing (..)
import Tracks exposing (IdentifiedTrack)
import UI.Types as UI



-- CREATE


create : List IdentifiedTrack -> List Playlist -> Alfred UI.Msg
create tracks playlists =
    let
        playlistNames =
            playlists
                |> List.map .name
                |> List.sortBy String.toLower
    in
    { action = createAction tracks
    , focus = 0
    , index = playlistNames
    , message =
        if List.length tracks == 1 then
            "Choose or create a playlist to add this track to."

        else
            "Choose or create a playlist to add these tracks to."
    , results = playlistNames
    , searchTerm = Nothing
    }


createAction : List IdentifiedTrack -> { result : Maybe String, searchTerm : Maybe String } -> List UI.Msg
createAction tracks maybe =
    let
        playlistTracks =
            Tracks.toPlaylistTracks tracks
    in
    case maybe.result of
        Just result ->
            -- Add to playlist
            --
            [ UI.AddTracksToPlaylist
                { playlistName = result
                , tracks = playlistTracks
                }
            ]

        Nothing ->
            -- Create playlist,
            -- if given a search term.
            --
            case maybe.searchTerm of
                Just searchTerm ->
                    [ UI.AddTracksToPlaylist
                        { playlistName = searchTerm
                        , tracks = playlistTracks
                        }
                    ]

                Nothing ->
                    []



-- SELECT


select : List Playlist -> Alfred UI.Msg
select playlists =
    let
        playlistNames =
            playlists
                |> List.map .name
                |> List.sortBy String.toLower
    in
    { action = selectAction playlists
    , focus = 0
    , index = playlistNames
    , message = "Select a playlist to play tracks from"
    , results = playlistNames
    , searchTerm = Nothing
    }


selectAction : List Playlist -> { result : Maybe String, searchTerm : Maybe String } -> List UI.Msg
selectAction playlists { result } =
    case Maybe.andThen (\r -> List.find (.name >> (==) r) playlists) result of
        Just playlist ->
            [ UI.SelectPlaylist playlist ]

        Nothing ->
            []
