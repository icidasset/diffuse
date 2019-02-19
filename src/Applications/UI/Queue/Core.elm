module UI.Queue.Core exposing (Model, Msg(..))

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
    = ------------------------------------
      -- Combos
      ------------------------------------
      InjectFirstAndPlay IdentifiedTrack
      ------------------------------------
      -- Future
      ------------------------------------
    | InjectFirst (List IdentifiedTrack)
    | InjectLast (List IdentifiedTrack)
      ------------------------------------
      -- Position
      ------------------------------------
    | Rewind
    | Shift
      ------------------------------------
      -- Contents
      ------------------------------------
    | Reset
    | Fill Time.Posix (List IdentifiedTrack)
      ------------------------------------
      -- Settings
      ------------------------------------
    | ToggleRepeat
    | ToggleShuffle
