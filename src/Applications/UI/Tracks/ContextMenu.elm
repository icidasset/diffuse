module UI.Tracks.ContextMenu exposing (trackMenu, viewMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Action as Icons
import Material.Icons.Maps as Icons
import Tracks exposing (IdentifiedTrack)
import UI.Core exposing (Msg(..))
import UI.Queue.Core as Queue



-- 🔱


trackMenu : List IdentifiedTrack -> Coordinates -> ContextMenu Msg
trackMenu tracks =
    ContextMenu (queueActions tracks)


viewMenu : Coordinates -> ContextMenu Msg
viewMenu =
    ContextMenu
        [ Item
            ( Icons.terrain
            , "Group by added-to-collection date"
            , Bypass
            )
        ]



-- ACTIONS


queueActions : List IdentifiedTrack -> List (ContextMenu.Item Msg)
queueActions identifiedTracks =
    [ Item
        ( Icons.event_seat
        , "Play next"
        , QueueMsg (Queue.InjectFirst { showNotification = True } identifiedTracks)
        )
    , Item
        ( Icons.event_seat
        , "Add to queue"
        , QueueMsg (Queue.InjectLast { showNotification = True } identifiedTracks)
        )
    ]
