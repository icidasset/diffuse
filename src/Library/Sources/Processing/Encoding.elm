module Sources.Processing.Encoding exposing (argumentsDecoder)

{-| Encoding.
-}

import Json.Decode as Decode exposing (Decoder)
import Sources.Encoding as Sources
import Sources.Processing exposing (Arguments)
import Tracks.Encoding as Tracks



-- 🔱


argumentsDecoder : Decoder Arguments
argumentsDecoder =
    Decode.map3 Arguments
        (Decode.field "origin" Decode.string)
        (Decode.field "sources" <| Decode.list Sources.decoder)
        (Decode.field "tracks" <| Decode.list Tracks.trackDecoder)
