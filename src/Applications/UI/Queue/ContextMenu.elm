module UI.Queue.ContextMenu exposing (futureMenu, historyMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons as Icons
import Queue
import UI.Queue.Types as Queue
import UI.Tracks.ContextMenu
import UI.Types exposing (Msg(..))



-- 🔱


futureMenu :
    { cached : List String, cachingInProgress : List String, itemIndex : Int }
    -> Queue.Item
    -> Coordinates
    -> ContextMenu Msg
futureMenu { cached, cachingInProgress, itemIndex } item =
    let
        tracks =
            [ item.identifiedTrack ]
    in
    ContextMenu
        [ Item
            { icon = Icons.update
            , label = "Move to the top"
            , msg =
                { index = itemIndex }
                    |> Queue.MoveItemToFirst
                    |> QueueMsg

            --
            , active = False
            }
        , Item
            { icon = Icons.update
            , label = "Move to the end of my picks"
            , msg =
                { index = itemIndex }
                    |> Queue.MoveItemToLast
                    |> QueueMsg

            --
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
    -> ContextMenu Msg
historyMenu { cached, cachingInProgress } item =
    let
        tracks =
            [ item.identifiedTrack ]
    in
    ContextMenu
        [ Item
            { icon = Icons.update
            , label = "Play next"
            , msg =
                { inFront = True, tracks = tracks }
                    |> Queue.AddTracks
                    |> QueueMsg

            --
            , active = False
            }
        , Item
            { icon = Icons.update
            , label = "Add to queue"
            , msg =
                { inFront = False, tracks = tracks }
                    |> Queue.AddTracks
                    |> QueueMsg

            --
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
