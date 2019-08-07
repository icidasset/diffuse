module Dict.Ext exposing (fetch, fetchUnknown, unionFlipped)

import Dict exposing (Dict)



-- 🔱


unionFlipped : Dict comparable v -> Dict comparable v -> Dict comparable v
unionFlipped a b =
    Dict.union b a


fetch : comparable -> v -> Dict comparable v -> v
fetch key default dict =
    dict
        |> Dict.get key
        |> Maybe.withDefault default


fetchUnknown : comparable -> Dict comparable String -> String
fetchUnknown key dict =
    fetch key "MISSING_VALUE" dict
