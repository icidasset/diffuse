module Tracks.ContextMenu exposing (..)

import Material.Icons.Av as Icons
import Material.Icons.Image as Icons
import Material.Icons.Editor as Icons
import Mouse
import Playlists.Types exposing (Msg(..))
import Playlists.Utils
import Queue.Types
import Tracks.Types exposing (..)
import Types exposing (..)
import Variables exposing (colorDerivatives)


trackMenu : Tracks.Types.Model -> Maybe String -> IdentifiedTrack -> Mouse.Position -> ContextMenu
trackMenu model lastModifiedPlaylist identifiedTrack =
    let
        track =
            Tuple.second identifiedTrack
    in
        [ queueActions identifiedTrack

        --
        --
        , case model.selectedPlaylist of
            -- Playlist actions, when in a playlist.
            --
            Just selectedPlaylist ->
                [ ( Icons.format_list_numbered colorDerivatives.text 16
                  , "Remove from playlist"
                  , identifiedTrack
                        |> Tuple.first
                        |> .indexInPlaylist
                        |> Maybe.withDefault 0
                        |> RemoveTrackByIndex selectedPlaylist.name
                        |> PlaylistsMsg
                  )
                , ( Icons.format_list_numbered colorDerivatives.text 16
                  , "Add to another playlist"
                  , RequestAssistanceForPlaylists [ track ]
                  )
                ]

            Nothing ->
                -- Playlist actions, default.
                defaultPlaylistActions track lastModifiedPlaylist
        ]
            |> List.concat
            |> ContextMenu



-- Actions


defaultPlaylistActions : Track -> Maybe String -> ContextMenuItems
defaultPlaylistActions track lastModifiedPlaylist =
    case lastModifiedPlaylist of
        Just playlistName ->
            [ ( Icons.format_list_numbered colorDerivatives.text 16
              , "Add to \"" ++ playlistName ++ "\""
              , track
                    |> List.singleton
                    |> List.map Playlists.Utils.playlistTrackFromTrack
                    |> AddToPlaylist playlistName
                    |> PlaylistsMsg
              )
            , ( Icons.format_list_numbered colorDerivatives.text 16
              , "Add to another playlist"
              , RequestAssistanceForPlaylists [ track ]
              )
            ]

        Nothing ->
            [ ( Icons.format_list_numbered colorDerivatives.text 16
              , "Add to playlist"
              , RequestAssistanceForPlaylists [ track ]
              )
            ]


queueActions : IdentifiedTrack -> ContextMenuItems
queueActions identifiedTrack =
    [ ( Icons.queue_play_next colorDerivatives.text 16
      , "Play next"
      , QueueMsg (Queue.Types.InjectFirst identifiedTrack { showNotification = True })
      )
    , ( Icons.add_to_queue colorDerivatives.text 16
      , "Add to queue"
      , QueueMsg (Queue.Types.InjectLast identifiedTrack { showNotification = True })
      )
    ]
