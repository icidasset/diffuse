module UI.Queue.ContextMenu exposing (futureItemMenu)

import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Material.Icons.Action as Icons
import Queue
import Sources exposing (Source)
import UI.Core exposing (Msg(..))



-- 🔱


futureItemMenu : Queue.Item -> Coordinates -> ContextMenu Msg
futureItemMenu item =
    ContextMenu
        []
