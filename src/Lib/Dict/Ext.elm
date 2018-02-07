module Dict.Ext exposing (..)

import Dict exposing (Dict)


unionF : Dict comparable v -> Dict comparable v -> Dict comparable v
unionF =
    flip Dict.union


fetch : comparable -> v -> Dict comparable v -> v
fetch key default dict =
    Dict.get key dict
        |> Maybe.withDefault default


fetchUnknown : comparable -> Dict comparable String -> String
fetchUnknown key dict =
    fetch key "missingValue" dict
