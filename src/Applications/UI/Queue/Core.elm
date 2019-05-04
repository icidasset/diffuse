module UI.Queue.Core exposing (Model, Msg(..))

import Html.Events.Extra.Mouse as Mouse
import Queue exposing (..)
import Time
import Tracks exposing (IdentifiedTrack)



-- 🌳


type alias Model =
    { activeItem : Maybe Item
    , future : List Item
    , ignored : List Item
    , past : List Item

    --
    , repeat : Bool
    , shuffle : Bool
    }



-- 📣


type Msg
    = ShowFutureItemMenu Item Mouse.Event
      ------------------------------------
      -- Combos
      ------------------------------------
    | InjectFirstAndPlay IdentifiedTrack
      ------------------------------------
      -- Future
      ------------------------------------
    | InjectFirst { showNotification : Bool } (List IdentifiedTrack)
    | InjectLast { showNotification : Bool } (List IdentifiedTrack)
    | RemoveItem { index : Int, item : Item }
      ------------------------------------
      -- Position
      ------------------------------------
    | Rewind
    | Shift
      ------------------------------------
      -- Contents
      ------------------------------------
    | Clear
    | Reset
    | Fill Time.Posix (List IdentifiedTrack)
      ------------------------------------
      -- Settings
      ------------------------------------
    | ToggleRepeat
    | ToggleShuffle
