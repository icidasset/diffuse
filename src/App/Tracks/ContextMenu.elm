module Tracks.ContextMenu exposing (build)

import Color
import Material.Icons.Av as Icons
import Mouse
import Queue.Types
import Tracks.Types exposing (..)
import Types exposing (..)


build : IdentifiedTrack -> (Mouse.Position -> ContextMenu)
build identifiedTrack =
    let
        track =
            Tuple.second identifiedTrack
    in
        ContextMenu
            [ ( Icons.queue_play_next (Color.rgb 0 0 0) 16
              , "Play next"
              , QueueMsg (Queue.Types.InjectFirst track)
              )
            , ( Icons.add_to_queue (Color.rgb 0 0 0) 16
              , "Add to queue"
              , QueueMsg (Queue.Types.InjectLast track)
              )
            ]
