module UI.Tracks.ContextMenu exposing (trackMenu, viewMenu)

import Conditional exposing (ifThenElse)
import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Action as Icons
import Material.Icons.Content as Icons
import Material.Icons.Maps as Icons
import Tracks exposing (Grouping(..), IdentifiedTrack)
import UI.Core exposing (Msg(..))
import UI.Queue.Core as Queue



-- TRACK MENU


trackMenu : List IdentifiedTrack -> Coordinates -> ContextMenu Msg
trackMenu tracks =
    ContextMenu (queueActions tracks)


queueActions : List IdentifiedTrack -> List (ContextMenu.Item Msg)
queueActions identifiedTracks =
    [ Item
        { icon = Icons.event_seat
        , label = "Play next"
        , msg = QueueMsg (Queue.InjectFirst { showNotification = True } identifiedTracks)
        , active = False
        }
    , Item
        { icon = Icons.event_seat
        , label = "Add to queue"
        , msg = QueueMsg (Queue.InjectLast { showNotification = True } identifiedTracks)
        , active = False
        }
    ]



-- VIEW MENU


viewMenu : Maybe Grouping -> Coordinates -> ContextMenu Msg
viewMenu maybeGrouping =
    ContextMenu
        [ groupByProcessingDate (maybeGrouping == Just AddedOnGroups)
        , groupByTrackYear (maybeGrouping == Just TrackYearGroups)
        ]


groupByProcessingDate isActive =
    Item
        { icon = ifThenElse isActive Icons.clear Icons.terrain
        , label = "Group by processing date"
        , msg = Bypass
        , active = isActive
        }


groupByTrackYear isActive =
    Item
        { icon = ifThenElse isActive Icons.clear Icons.terrain
        , label = "Group by track year"
        , msg = Bypass
        , active = isActive
        }
