module Queue.Types exposing (..)

import Date exposing (Date)
import DnD
import Tracks.Types exposing (IdentifiedTrack, Track)


-- Messages


type Msg
    = InjectFirst IdentifiedTrack InjectOptions
    | InjectLast (List IdentifiedTrack) InjectOptions
    | RemoveItem Int
      -- Position
    | Rewind
    | Shift
      -- Contents
    | Fill Date (List IdentifiedTrack)
    | Clear
    | Clean (List IdentifiedTrack)
    | Reset
      -- Combos
    | InjectFirstAndPlay IdentifiedTrack
      -- Settings
    | ToggleRepeat
    | ToggleShuffle
      -- UI
    | DragItemMsg (DnD.Msg Int)



-- Model


type alias Model =
    InternalModel Settings


type alias InternalModel extension =
    { extension
        | activeItem : Maybe Item
        , future : List Item
        , ignored : List Item
        , past : List Item

        -- UI
        , itemDnD : DnD.Model Int
    }


type alias Settings =
    { repeat : Bool
    , shuffle : Bool
    }



-- Items


type alias Item =
    { manualEntry : Bool
    , identifiedTrack : IdentifiedTrack
    }


type alias EngineItem =
    { track : Track
    , url : String
    }



-- Routing


type Page
    = Index
    | History



-- Other


type alias InjectOptions =
    { showNotification : Bool }
