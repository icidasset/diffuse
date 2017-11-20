module Tracks.ContextMenu exposing (..)

import Color
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


trackMenu : Maybe String -> IdentifiedTrack -> Mouse.Position -> ContextMenu
trackMenu maybeLastModifiedPlaylist identifiedTrack =
    let
        track =
            Tuple.second identifiedTrack
    in
        [ queueActions track
        , playlistActions track maybeLastModifiedPlaylist
        ]
            |> List.concat
            |> ContextMenu



-- Actions


playlistActions : Track -> Maybe String -> ContextMenuItems
playlistActions track maybeLastModifiedPlaylist =
    case maybeLastModifiedPlaylist of
        Just playlistName ->
            [ ( Icons.format_list_numbered colorDerivatives.text 16
              , "Add to the `" ++ playlistName ++ "` playlist"
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


queueActions : Track -> ContextMenuItems
queueActions track =
    [ ( Icons.queue_play_next colorDerivatives.text 16
      , "Play next"
      , QueueMsg (Queue.Types.InjectFirst track { showNotification = True })
      )
    , ( Icons.add_to_queue colorDerivatives.text 16
      , "Add to queue"
      , QueueMsg (Queue.Types.InjectLast track { showNotification = True })
      )
    ]
