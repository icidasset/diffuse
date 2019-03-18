module Equalizer exposing (Settings, defaultSettings, encodeSettings, settingsDecoder)

import Json.Decode
import Json.Encode



-- 🌳


type alias Settings =
    { low : Float
    , mid : Float
    , high : Float
    , volume : Float
    }



-- 🔱


defaultSettings : Settings
defaultSettings =
    { low = 0
    , mid = 0
    , high = 0
    , volume = 0.5
    }


encodeSettings : Settings -> Json.Encode.Value
encodeSettings settings =
    Json.Encode.object
        [ ( "low", Json.Encode.float settings.low )
        , ( "mid", Json.Encode.float settings.mid )
        , ( "high", Json.Encode.float settings.high )
        , ( "volume", Json.Encode.float settings.volume )
        ]


settingsDecoder : Json.Decode.Decoder Settings
settingsDecoder =
    Json.Decode.map4
        Settings
        (Json.Decode.field "low" Json.Decode.float)
        (Json.Decode.field "mid" Json.Decode.float)
        (Json.Decode.field "high" Json.Decode.float)
        (Json.Decode.field "volume" Json.Decode.float)
