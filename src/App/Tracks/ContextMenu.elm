module Tracks.ContextMenu exposing (..)

import Color
import Material.Icons.Av as Icons
import Material.Icons.Image as Icons
import Material.Icons.Editor as Icons
import Mouse
import Queue.Types
import Tracks.Types exposing (..)
import Types exposing (..)
import Variables exposing (colorDerivatives)


trackMenu : IdentifiedTrack -> Mouse.Position -> ContextMenu
trackMenu identifiedTrack =
    let
        track =
            Tuple.second identifiedTrack
    in
        ContextMenu
            [ ( Icons.queue_play_next colorDerivatives.text 16
              , "Play next"
              , QueueMsg (Queue.Types.InjectFirst track { showNotification = True })
              )
            , ( Icons.add_to_queue colorDerivatives.text 16
              , "Add to queue"
              , QueueMsg (Queue.Types.InjectLast track { showNotification = True })
              )
            , ( Icons.format_list_numbered colorDerivatives.text 16
              , "Add to playlist"
              , RequestAssistanceForPlaylists [ track ]
              )
            ]
