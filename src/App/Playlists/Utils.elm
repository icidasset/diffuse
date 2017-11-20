module Playlists.Utils exposing (..)

import Playlists.Types exposing (Playlist, PlaylistTrack)
import Sources.Types exposing (Source)
import Tracks.Types exposing (Track)


{-| Auto-generate directory playlists.
-}
autoGenerate : List Source -> List Track -> List Playlist
autoGenerate sources tracks =
    let
        relevantSources =
            List.filter .directoryPlaylists sources

        relevantSourceIds =
            List.map .id relevantSources

        relevantTracks =
            List.filter
                (\t -> List.member t.sourceId relevantSourceIds)
                tracks

        playlistNames =
            List.foldr
                (\t acc ->
                    let
                        name =
                            String.split "/" t.path
                                |> List.head
                                |> Maybe.withDefault ""
                    in
                        if List.member name acc == False then
                            name :: acc
                        else
                            acc
                )
                []
                relevantTracks
    in
        List.map
            (\n ->
                { autoGenerated = True
                , name = n
                , tracks = []
                }
            )
            playlistNames


{-| Create a `PlaylistTrack` from a `Track`.
-}
playlistTrackFromTrack : Track -> PlaylistTrack
playlistTrackFromTrack track =
    { album = track.tags.album
    , artist = track.tags.artist
    , title = track.tags.title
    }
