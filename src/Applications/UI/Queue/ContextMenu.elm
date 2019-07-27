module UI.Queue.ContextMenu exposing (futureMenu, historyMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Action as Icons
import Material.Icons.Content as Icons
import Queue
import UI.Reply exposing (Reply(..))
import UI.Tracks.ContextMenu



-- 🔱


futureMenu :
    { cached : List String, cachingInProgress : List String, itemIndex : Int }
    -> Queue.Item
    -> Coordinates
    -> ContextMenu Reply
futureMenu { cached, cachingInProgress, itemIndex } item =
    let
        tracks =
            [ item.identifiedTrack ]
    in
    ContextMenu
        [ Item
            { icon = Icons.update
            , label = "Move to the top"
            , msg = MoveQueueItemToFirst { itemIndex = itemIndex }
            , active = False
            }
        , Item
            { icon = Icons.update
            , label = "Move to the end of my picks"
            , msg = MoveQueueItemToLast { itemIndex = itemIndex }
            , active = False
            }
        , Item
            { icon = Icons.waves
            , label = "Add to playlist"
            , msg = RequestAssistanceForPlaylists tracks
            , active = False
            }
        , UI.Tracks.ContextMenu.cacheAction
            { cached = cached, cachingInProgress = cachingInProgress }
            tracks
        ]


historyMenu :
    { cached : List String, cachingInProgress : List String }
    -> Queue.Item
    -> Coordinates
    -> ContextMenu Reply
historyMenu { cached, cachingInProgress } item =
    let
        tracks =
            [ item.identifiedTrack ]
    in
    ContextMenu
        [ Item
            { icon = Icons.update
            , label = "Play next"
            , msg = AddToQueue { inFront = True, tracks = tracks }
            , active = False
            }
        , Item
            { icon = Icons.update
            , label = "Add to queue"
            , msg = AddToQueue { inFront = False, tracks = tracks }
            , active = False
            }
        , Item
            { icon = Icons.waves
            , label = "Add to playlist"
            , msg = RequestAssistanceForPlaylists tracks
            , active = False
            }
        , UI.Tracks.ContextMenu.cacheAction
            { cached = cached, cachingInProgress = cachingInProgress }
            tracks
        ]
