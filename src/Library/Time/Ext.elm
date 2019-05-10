module Time.Ext exposing (decoder, default, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json
import Time



-- 🔱


decoder : Decoder Time.Posix
decoder =
    Decode.map Time.millisToPosix Decode.int


default : Time.Posix
default =
    Time.millisToPosix 0


encode : Time.Posix -> Json.Value
encode time =
    Json.int (Time.posixToMillis time)
