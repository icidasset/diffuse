module UI.Queue.Types exposing (..)

import Html.Events.Extra.Mouse as Mouse
import Queue exposing (Item)
import Time
import Tracks exposing (IdentifiedTrack)
import UI.DnD as DnD



-- 📣


type Msg
    = Clear
    | Reset
    | Rewind
    | Select Item
    | Shift
    | ShowFutureMenu Item { index : Int } Mouse.Event
    | ShowHistoryMenu Item Mouse.Event
    | ToggleRepeat
    | ToggleShuffle
      ------------------------------------
      -- Future
      ------------------------------------
    | InjectFirst { showNotification : Bool } (List IdentifiedTrack)
    | InjectLast { showNotification : Bool } (List IdentifiedTrack)
    | InjectFirstAndPlay IdentifiedTrack
    | RemoveItem { index : Int, item : Item }
