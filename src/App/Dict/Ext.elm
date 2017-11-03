module Dict.Ext exposing (..)

import Dict exposing (Dict)


fetch : k -> v -> Dict k v -> v
fetch key default dict =
    Dict.get key dict
        |> Maybe.withDefault default


fetchUnknown : k -> Dict k String -> String
fetchUnknown key dict =
    fetch key "missingValue" dict
