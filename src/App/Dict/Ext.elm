module Dict.Ext exposing (..)

import Dict exposing (Dict)


fetch : String -> String -> Dict String String -> String
fetch key default dict =
    Dict.get key dict
        |> Maybe.withDefault default


fetchUnknown : String -> Dict String String -> String
fetchUnknown key dict =
    fetch key "missingValue" dict
