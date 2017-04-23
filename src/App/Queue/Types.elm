module Queue.Types exposing (Msg(..), Model, Settings, Item, EngineItem, Page(..))

import Date exposing (Date)
import Sources.Types exposing (Source)
import Tracks.Types exposing (Track, TrackId)


-- Types


type Msg
    = InjectFirst Track
    | InjectLast Track
    | RemoveItem Int
      -- Position
    | Rewind
    | Shift
      -- Contents
    | Fill Date (List Track)
    | Clean (List Track)
    | Reset
      -- Combos
    | InjectFirstAndPlay Track
      -- Settings
    | ToggleRepeat
    | ToggleShuffle


type alias Model =
    InternalModel Settings


type alias Settings =
    { repeat : Bool
    , shuffle : Bool
    }


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



-- Private


type alias InternalModel extension =
    { extension
        | activeItem : Maybe Item
        , future : List Item
        , past : List Item
    }
