module Dict.Ext exposing (fetch, fetchUnknown)

import Dict exposing (Dict)


fetch : comparable -> v -> Dict comparable v -> v
fetch key default dict =
    dict
        |> Dict.get key
        |> Maybe.withDefault default


fetchUnknown : comparable -> Dict comparable String -> String
fetchUnknown key dict =
    fetch key "MISSING_VALUE" dict
