module Abroad.Types exposing (..)

-- Messages


type Msg
    = FileSelectedForImport
    | Import
    | ImportFinished (Maybe String)



-- Model


type alias Model =
    { fileContents : Maybe String
    , fileSelected : Bool
    , importMessage : Result String String
    }
