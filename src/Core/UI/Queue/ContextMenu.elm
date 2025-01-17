module UI.Queue.ContextMenu exposing (futureMenu, futureNavigationMenu, historyMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Round as Icons
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
            , label = "Add to collection"
            , msg = AssistWithAddingTracksToCollection tracks
            , active = False
            }
        , Item
            { icon = Icons.waves
            , label = "Add to playlist"
            , msg = AssistWithAddingTracksToPlaylist tracks
            , active = False
            }
        , UI.Tracks.ContextMenu.cacheAction
            { cached = cached, cachingInProgress = cachingInProgress }
            tracks
        ]


futureNavigationMenu : { manualEntries : List Queue.Item } -> Coordinates -> ContextMenu Msg
futureNavigationMenu { manualEntries } =
    [ [ Item
            { icon = Icons.not_interested
            , label = "Reset ignored"
            , msg = QueueMsg Queue.ResetIgnored

            --
            , active = False
            }
      ]
    , --
      if List.isEmpty manualEntries then
        []

      else
        [ Item
            { icon = Icons.waves
            , label = "Add queue picks to collection"
            , msg =
                manualEntries
                    |> List.map .identifiedTrack
                    |> AssistWithAddingTracksToCollection
            , active = False
            }
        , Item
            { icon = Icons.waves
            , label = "Add queue picks to playlist"
            , msg =
                manualEntries
                    |> List.map .identifiedTrack
                    |> AssistWithAddingTracksToPlaylist
            , active = False
            }
        ]
    ]
        |> List.concat
        |> ContextMenu


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
            , label = "Add to collection"
            , msg = AssistWithAddingTracksToCollection tracks
            , active = False
            }
        , Item
            { icon = Icons.waves
            , label = "Add to playlist"
            , msg = AssistWithAddingTracksToPlaylist tracks
            , active = False
            }
        , UI.Tracks.ContextMenu.cacheAction
            { cached = cached, cachingInProgress = cachingInProgress }
            tracks
        ]
