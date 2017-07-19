module Queue.Types exposing (..)

import Date exposing (Date)
import Sources.Types exposing (Source)
import Tracks.Types exposing (Track, TrackId)


-- Messages


type Msg
    = InjectFirst Track
    | InjectLast Track
    | RemoveItem Int
      -- Position
    | Rewind
    | Shift
      -- Contents
    | Fill Date (List Track)
    | Clear
    | Clean (List Track)
    | Reset
      -- Combos
    | InjectFirstAndPlay Track
      -- Settings
    | ToggleRepeat
    | ToggleShuffle



-- Model


type alias Model =
    InternalModel Settings


type alias InternalModel extension =
    { extension
        | activeItem : Maybe Item
        , future : List Item
        , past : List Item
    }


type alias Settings =
    { repeat : Bool
    , shuffle : Bool
    }



-- Items


type alias Item =
    { manualEntry : Bool
    , track : Track
    }


type alias EngineItem =
    { track : Track
    , url : String
    }



-- Routing


type Page
    = Index
    | History
