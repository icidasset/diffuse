module Settings.Types exposing (..)

-- Messages


type Msg
    = SetChosenBackdrop String
    | SetLoadedBackdrop String



-- Model


type alias Model =
    { chosenBackdrop : String
    , fadeInLastBackdrop : Bool
    , loadedBackdrops : List String
    }
