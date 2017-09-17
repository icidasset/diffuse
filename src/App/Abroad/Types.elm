module Abroad.Types exposing (..)

import FileReader


-- Messages


type Msg
    = UploadFiles
    | ReadFile (Result FileReader.Error String)
    | SetFiles (List FileReader.NativeFile)



-- Model


type alias Model =
    { files : List FileReader.NativeFile
    , importMessage : Result String String
    }
