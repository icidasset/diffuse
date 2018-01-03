module Playlists.Alfred exposing (..)

import Playlists.Types exposing (Msg(..), Playlist, PlaylistTrack)
import Playlists.Utils
import Response.Ext exposing (do)
import Tracks.Types exposing (Track)
import Types as TopLevel exposing (Alfred)


-- 🍯


create : List Track -> List Playlist -> Alfred
create tracks playlists =
    let
        playlistNames =
            playlists
                |> List.map .name
                |> List.sort
    in
        { action = tracks |> List.map Playlists.Utils.playlistTrackFromTrack |> action
        , focus = 0
        , index = playlistNames
        , message = "Choose or create a playlist to add these tracks to."
        , results = playlistNames
        , searchTerm = Nothing
        }



-- Action


action : List PlaylistTrack -> Maybe String -> Maybe String -> Cmd TopLevel.Msg
action tracks maybeSearchTerm maybeResult =
    case maybeResult of
        Just result ->
            -- Add to playlist
            --
            Cmd.batch
                [ tracks
                    |> AddToPlaylist result
                    |> TopLevel.PlaylistsMsg
                    |> do
                , TopLevel.HideAlfred
                    |> do
                ]

        Nothing ->
            -- Create playlist,
            -- if given a search term.
            --
            case maybeSearchTerm of
                Just searchTerm ->
                    Cmd.batch
                        [ tracks
                            |> CreateWithTracks searchTerm
                            |> TopLevel.PlaylistsMsg
                            |> do
                        , TopLevel.HideAlfred
                            |> do
                        ]

                Nothing ->
                    Cmd.none
