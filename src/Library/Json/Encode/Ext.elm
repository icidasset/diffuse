module Json.Encode.Ext exposing (..)

import Json.Encode as Encode


encodeMaybe : Maybe a -> (a -> Encode.Value) -> Encode.Value
encodeMaybe maybe encoder =
    maybe
        |> Maybe.map encoder
        |> Maybe.withDefault Encode.null
