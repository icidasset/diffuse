module Queue.Types exposing (Msg(..), Model, Settings, Item, Page(..))

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
    | Fill (List Source) (List Track)
    | FillStepTwo (List Source) (List Track) (List Track)
    | Reset
      -- Combos
    | InjectFirstAndPlay Item
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
    { id : String
    , manualEntry : Bool
    , track : Track
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

        --
        , timestamp : Date
    }
