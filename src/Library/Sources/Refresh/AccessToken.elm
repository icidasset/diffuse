module Sources.Refresh.AccessToken exposing (..)

import Json.Decode exposing (Decoder)
import Sources exposing (Service)
import Sources.Encoding exposing (serviceDecoder)



-- 🌳


type alias PortArguments =
    { service : Service
    , sourceId : String
    , accessToken : String

    -- Unix timestamp in milliseconds
    , expiresAt : Int
    }



-- 🛠


portArgumentsDecoder : Decoder PortArguments
portArgumentsDecoder =
    Json.Decode.map4
        (\service sourceId accessToken expiresAt ->
            { service = service
            , sourceId = sourceId
            , accessToken = accessToken
            , expiresAt = expiresAt
            }
        )
        (Json.Decode.field "service" serviceDecoder)
        (Json.Decode.field "sourceId" Json.Decode.string)
        (Json.Decode.field "accessToken" Json.Decode.string)
        (Json.Decode.field "expiresAt" Json.Decode.int)
