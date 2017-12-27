module Dict.Ext exposing (..)

import Dict exposing (Dict)


fetch : comparable -> v -> Dict comparable v -> v
fetch key default dict =
    Dict.get key dict
        |> Maybe.withDefault default


fetchUnknown : comparable -> Dict comparable String -> String
fetchUnknown key dict =
    fetch key "missingValue" dict
