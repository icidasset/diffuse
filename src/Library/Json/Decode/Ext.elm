module Json.Decode.Ext exposing (listIgnore)

import Json.Decode exposing (Decoder)
import Maybe.Extra as Maybe



-- 🔱


{-| A list decoder that always succeeds, throwing away the failures.
-}
listIgnore : Decoder a -> Decoder (List a)
listIgnore decoder =
    decoder
        |> Json.Decode.maybe
        |> Json.Decode.list
        |> Json.Decode.map Maybe.values
