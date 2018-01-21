module Playlists.Utils exposing (..)

import Playlists.Types exposing (..)
import Sources.Types exposing (Source)
import Tracks.Types exposing (Track)


{-| Create a `PlaylistTrack` from a `Track`.
-}
playlistTrackFromTrack : Track -> PlaylistTrack
playlistTrackFromTrack track =
    { album = track.tags.album
    , artist = track.tags.artist
    , title = track.tags.title
    }



-- Matchers


playlistTrackMatcher : PlaylistTrack -> PlaylistTrack -> Bool
playlistTrackMatcher a b =
    a.album == b.album && a.artist == b.artist && a.title == b.title


trackWithPlaylistTrackMatcher : Track -> PlaylistTrack -> Bool
trackWithPlaylistTrackMatcher track =
    track
        |> playlistTrackFromTrack
        |> playlistTrackMatcher


trackWithIdentifiedPlaylistTrackMatcher : Track -> IdentifiedPlaylistTrack -> Bool
trackWithIdentifiedPlaylistTrackMatcher track =
    Tuple.second >> trackWithPlaylistTrackMatcher track
