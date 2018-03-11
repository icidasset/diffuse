module Settings.Types exposing (..)

-- Messages


type Msg
    = SetChosenBackdrop String
    | SetDefaultBackdropIfNecessary
    | SetLoadedBackdrop String



-- Model


type alias Model =
    { chosenBackdrop : Maybe String
    , fadeInLastBackdrop : Bool
    , loadedBackdrops : List String
    }
