module Queue.Types exposing (Msg(..), Model, Settings, Item)

import Tracks.Types exposing (Track)


-- Types


type Msg
    = InjectFirst Item
    | InjectLast Item
    | RemoveItem Int
      -- Position
    | Rewind
    | Shift
      -- Contents
    | Fill
    | Reset
      -- Combos
    | InjectFirstAndPlay Item
      -- Settings
    | ToggleRepeat
    | ToggleShuffle
      -- Tracks
    | AddTracks (List Track)


type alias Model =
    InternalModel Settings


type alias Settings =
    { repeat : Bool
    , shuffle : Bool
    }


type alias Item =
    { id : String
    , manualEntry : Bool
    , track : Track
    , url : String
    }



-- Private


type alias InternalModel extension =
    { extension
        | activeItem : Maybe Item
        , future : List Item
        , past : List Item
        , tracks : List Track
    }
