module Settings.Types exposing (..)

-- Messages


type Msg
    = SetChosenBackdrop String
    | SetLoadedBackdrop String



-- Model


type alias Model =
    { chosenBackdrop : String
    , loadedBackdrop : LoadedBackdrop
    }


type alias LoadedBackdrop =
    { previous : Maybe String, current : Maybe String }
