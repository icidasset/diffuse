module Brain.User.Hypaethral exposing (..)

import Alien
import Brain.Task.Ports
import Json.Decode
import Json.Encode
import TaskPort
import User.Layer as User exposing (..)



-- RETRIEVAL


retrieveDropbox : String -> HypaethralBit -> TaskPort.Task (Maybe Json.Decode.Value)
retrieveDropbox accessToken bit =
    TaskPort.call
        { function = "fromDropbox"
        , valueDecoder = Json.Decode.maybe Json.Decode.value
        , argsEncoder = Json.Encode.object
        }
        [ ( "fileName", Json.Encode.string (hypaethralBitFileName bit) )
        , ( "token", Json.Encode.string accessToken )
        ]


retrieveLocal : HypaethralBit -> TaskPort.Task (Maybe Json.Decode.Value)
retrieveLocal bit =
    Brain.Task.Ports.fromCacheWithSuffix
        Alien.SyncLocal
        (hypaethralBitFileName bit)
        Json.Decode.value



-- STORAGE


saveDropbox : String -> HypaethralBit -> Json.Decode.Value -> TaskPort.Task ()
saveDropbox accessToken bit data =
    TaskPort.call
        { function = "toDropbox"
        , valueDecoder = TaskPort.ignoreValue
        , argsEncoder = Json.Encode.object
        }
        [ ( "fileName", Json.Encode.string (hypaethralBitFileName bit) )
        , ( "data", data )
        , ( "token", Json.Encode.string accessToken )
        ]


saveLocal : HypaethralBit -> Json.Decode.Value -> TaskPort.Task ()
saveLocal bit data =
    Brain.Task.Ports.toCacheWithSuffix
        Alien.SyncLocal
        (hypaethralBitFileName bit)
        data
