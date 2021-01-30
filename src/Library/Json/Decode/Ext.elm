module Json.Decode.Ext exposing (listIgnore, optionalField)

import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra as Maybe



-- 🔱


{-| A list decoder that always succeeds, throwing away the failures.
-}
listIgnore : Decoder a -> Decoder (List a)
listIgnore decoder =
    decoder
        |> Decode.maybe
        |> Decode.list
        |> Decode.map Maybe.values


{-| Provide a default value for a field that might not be there.
-}
optionalField : String -> Decoder a -> a -> Decoder a
optionalField field decoder defaultValue =
    decoder
        |> Decode.field field
        |> Decode.maybe
        |> Decode.map (Maybe.withDefault defaultValue)
