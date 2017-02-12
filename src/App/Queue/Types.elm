module Queue.Types exposing (Msg(..), Model, Settings, Item)


type Msg
    = InjectFirst Item
    | InjectLast Item
    | RemoveItem Int
      -- Position
    | Rewind
    | Reset
    | Shift
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
    { id : TrackId
    , manualEntry : Bool
    , url : String
    }



-- Private


type alias InternalModel extension =
    { extension
        | activeItem : Maybe Item
        , future : List Item
        , past : List Item
    }


type alias TrackId =
    Int
