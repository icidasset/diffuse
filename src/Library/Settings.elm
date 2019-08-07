module Settings exposing (Settings, decoder, encode)

import Json.Decode as Json
import Json.Decode.Pipeline exposing (optional)
import Json.Encode
import Maybe.Extra as Maybe



-- 🌳


type alias Settings =
    { backgroundImage : Maybe String
    , hideDuplicates : Bool
    , processAutomatically : Bool
    , rememberProgress : Bool
    }



-- 🔱


encode : Settings -> Json.Value
encode settings =
    Json.Encode.object
        [ ( "backgroundImage"
          , Maybe.unwrap Json.Encode.null Json.Encode.string settings.backgroundImage
          )
        , ( "hideDuplicates"
          , Json.Encode.bool settings.hideDuplicates
          )
        , ( "processAutomatically"
          , Json.Encode.bool settings.processAutomatically
          )
        , ( "rememberProgress"
          , Json.Encode.bool settings.rememberProgress
          )
        ]


decoder : Json.Decoder Settings
decoder =
    Json.succeed Settings
        |> optional "backgroundImage" (Json.maybe Json.string) Nothing
        |> optional "hideDuplicates" Json.bool False
        |> optional "processAutomatically" Json.bool True
        |> optional "rememberProgress" Json.bool True
