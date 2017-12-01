module Queue.Types exposing (..)

import Date exposing (Date)
import Html5.DragDrop as DragDrop
import Sources.Types exposing (Source)
import Tracks.Types exposing (IdentifiedTrack, Track, TrackId)


-- Messages


type Msg
    = InjectFirst IdentifiedTrack InjectOptions
    | InjectLast IdentifiedTrack InjectOptions
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
      -- Libraries
    | DragDropMsg (DragDrop.Msg Int Int)



-- Model


type alias Model =
    InternalModel Settings


type alias InternalModel extension =
    { extension
        | activeItem : Maybe Item
        , dnd : DragDrop.Model Int Int
        , future : List Item
        , ignored : List Item
        , past : List Item
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
