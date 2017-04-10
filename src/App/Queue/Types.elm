module Queue.Types exposing (Msg(..), Model, Settings, Item)

import Date exposing (Date)
import Sources.Types exposing (Source)
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
    | Fill (List Source)
    | FillStepTwo (List Source) (List Track)
    | Reset
      -- Combos
    | InjectFirstAndPlay Item
      -- Settings
    | ToggleRepeat
    | ToggleShuffle
      -- Tracks
    | AddTracks (List Track)
    | RemoveTracks SourceId
    | RemoveTracksByPaths SourceId (List String)


type alias Model =
    InternalModel Settings


type alias SourceId =
    String


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

        --
        , timestamp : Date
        , tracks : List Track
    }
