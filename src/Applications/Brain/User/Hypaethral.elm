module Brain.User.Hypaethral exposing (..)

import Alien
import Brain.Task.Ports
import Json.Decode
import Json.Encode
import Task exposing (Task)
import TaskPort
import TaskPort.Extra as TaskPort
import User.Layer as User exposing (..)



-- RETRIEVAL


retrieveDropbox : String -> HypaethralBit -> Task String (Maybe Json.Decode.Value)
retrieveDropbox accessToken bit =
    [ ( "fileName", Json.Encode.string (hypaethralBitFileName bit) )
    , ( "token", Json.Encode.string accessToken )
    ]
        |> TaskPort.call
            { function = "fromDropbox"
            , valueDecoder = Json.Decode.maybe Json.Decode.value
            , argsEncoder = Json.Encode.object
            }
        |> Task.mapError TaskPort.errorToStringCustom


retrieveLocal : HypaethralBit -> Task String (Maybe Json.Decode.Value)
retrieveLocal bit =
    Json.Decode.value
        |> Brain.Task.Ports.fromCacheWithSuffix
            Alien.SyncLocal
            (hypaethralBitFileName bit)
        |> Task.mapError TaskPort.errorToStringCustom



-- STORAGE


saveDropbox : String -> HypaethralBit -> Json.Decode.Value -> Task String ()
saveDropbox accessToken bit data =
    [ ( "fileName", Json.Encode.string (hypaethralBitFileName bit) )
    , ( "data", data )
    , ( "token", Json.Encode.string accessToken )
    ]
        |> TaskPort.call
            { function = "toDropbox"
            , valueDecoder = TaskPort.ignoreValue
            , argsEncoder = Json.Encode.object
            }
        |> Task.mapError TaskPort.errorToStringCustom


saveLocal : HypaethralBit -> Json.Decode.Value -> Task String ()
saveLocal bit data =
    data
        |> Brain.Task.Ports.toCacheWithSuffix
            Alien.SyncLocal
            (hypaethralBitFileName bit)
        |> Task.mapError TaskPort.errorToStringCustom
