module UI.Tracks.ContextMenu exposing (trackMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Action as Icons
import Tracks exposing (IdentifiedTrack)
import UI.Core exposing (Msg(..))
import UI.Queue.Core as Queue



-- 🔱


trackMenu : List IdentifiedTrack -> Coordinates -> ContextMenu Msg
trackMenu tracks =
    ContextMenu (queueActions tracks)



-- ACTIONS


queueActions : List IdentifiedTrack -> ContextMenuItems Msg
queueActions identifiedTracks =
    [ ( Icons.event_seat
      , "Play next"
      , QueueMsg (Queue.InjectFirst identifiedTracks)
      )
    , ( Icons.event_seat
      , "Add to queue"
      , QueueMsg (Queue.InjectLast identifiedTracks)
      )
    ]
