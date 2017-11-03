module Tracks.ContextMenu exposing (..)

import Color
import Material.Icons.Av as Icons
import Material.Icons.Image as Icons
import Mouse
import Queue.Types
import Tracks.Types exposing (..)
import Types exposing (..)


trackMenu : IdentifiedTrack -> Mouse.Position -> ContextMenu
trackMenu identifiedTrack =
    let
        track =
            Tuple.second identifiedTrack
    in
        ContextMenu
            [ ( Icons.queue_play_next (Color.rgb 65 50 63) 16
              , "Play next"
              , QueueMsg (Queue.Types.InjectFirst track)
              )
            , ( Icons.add_to_queue (Color.rgb 65 50 63) 16
              , "Add to queue"
              , QueueMsg (Queue.Types.InjectLast track)
              )
            ]
