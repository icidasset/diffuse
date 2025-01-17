module Playlists.Matching exposing (match)

import Playlists exposing (..)
import Tracks exposing (IdentifiedTrack)



-- 🔱


match : Playlist -> List IdentifiedTrack -> ( List IdentifiedTrack, List IdentifiedPlaylistTrack )
match playlist =
    List.foldl
        (\( i, t ) ( identifiedTracks, remainingPlaylistTracks ) ->
            let
                im =
                    { album = t.tags.album
                    , artist = t.tags.artist
                    , title = t.tags.title
                    }

                ( matches, remainingPlaylistTracksWithoutMatches ) =
                    List.foldl
                        (\( pi, pt ) ->
                            if im.title == pt.title && im.album == pt.album && im.artist == pt.artist then
                                Tuple.mapBoth
                                    ((::) ( playlistTrackIdentifiers i pi, t ))
                                    identity

                            else
                                Tuple.mapBoth
                                    identity
                                    ((::) ( pi, pt ))
                        )
                        ( [], [] )
                        remainingPlaylistTracks
            in
            ( identifiedTracks ++ matches
            , remainingPlaylistTracksWithoutMatches
            )
        )
        ( []
        , List.indexedMap (\idx -> Tuple.pair { index = idx }) playlist.tracks
        )



-- ㊙️


playlistTrackIdentifiers : Tracks.Identifiers -> Playlists.Identifiers -> Tracks.Identifiers
playlistTrackIdentifiers i pi =
    { i | indexInPlaylist = Just pi.index }
