module UI.Playlists.Alfred exposing (action, create)

import Alfred exposing (..)
import Playlists exposing (..)
import Tracks exposing (IdentifiedTrack)
import UI.Reply exposing (Reply(..))



-- 🔱


create : List IdentifiedTrack -> List Playlist -> Alfred Reply
create tracks playlists =
    let
        playlistNames =
            playlists
                |> List.map .name
                |> List.sort
    in
    { action = action tracks
    , focus = 0
    , index = playlistNames
    , message =
        if List.length tracks == 1 then
            "Choose or create a playlist to add this track to."

        else
            "Choose or create a playlist to add these tracks to."
    , results = List.sort playlistNames
    , searchTerm = Nothing
    }


action : List IdentifiedTrack -> { result : Maybe String, searchTerm : Maybe String } -> List Reply
action tracks maybe =
    let
        playlistTracks =
            Tracks.toPlaylistTracks tracks
    in
    case maybe.result of
        Just result ->
            -- Add to playlist
            --
            [ AddTracksToPlaylist { playlistName = result, tracks = playlistTracks } ]

        Nothing ->
            -- Create playlist,
            -- if given a search term.
            --
            case maybe.searchTerm of
                Just searchTerm ->
                    [ AddTracksToPlaylist { playlistName = searchTerm, tracks = playlistTracks } ]

                Nothing ->
                    []
