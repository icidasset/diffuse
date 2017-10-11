port module Abroad.Ports exposing (..)

-- 💡


importFileInputId : String
importFileInputId =
    "importFileInput"


port importData : String -> Cmd msg



-- 🚽


port importDataReady : (Maybe String -> msg) -> Sub msg
