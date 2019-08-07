module Sources.Processing.Encoding exposing (argumentsDecoder)

{-| Encoding.
-}

import Json.Decode as Decode exposing (Decoder)
import Sources.Encoding as Sources
import Sources.Processing exposing (Arguments)



-- 🔱


argumentsDecoder : Decoder Arguments
argumentsDecoder =
    Decode.map2 Arguments
        (Decode.field "origin" Decode.string)
        (Decode.field "sources" <| Decode.list Sources.decoder)
