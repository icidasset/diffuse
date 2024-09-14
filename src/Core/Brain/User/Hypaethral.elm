module Brain.User.Hypaethral exposing (..)

import Alien
import Brain.Task.Ports
import Json.Decode
import Json.Encode
import Task exposing (Task)
import TaskPort
import TaskPort.Extra as TaskPort
import User.Layer exposing (..)



-- RETRIEVAL


retrieveDropbox : String -> HypaethralBit -> Task String (Maybe Json.Decode.Value)
retrieveDropbox accessToken bit =
    [ ( "fileName", fileName bit )
    , ( "token", Json.Encode.string accessToken )
    ]
        |> TaskPort.call
            { function = "fromDropbox"
            , valueDecoder = Json.Decode.maybe Json.Decode.value
            , argsEncoder = Json.Encode.object
            }
        |> Task.mapError TaskPort.errorToStringCustom


retrieveIpfs : String -> HypaethralBit -> Task String (Maybe Json.Decode.Value)
retrieveIpfs apiOrigin bit =
    [ ( "fileName", fileName bit )
    , ( "apiOrigin", Json.Encode.string apiOrigin )
    ]
        |> TaskPort.call
            { function = "fromIpfs"
            , valueDecoder = Json.Decode.maybe Json.Decode.value
            , argsEncoder = Json.Encode.object
            }
        |> Task.mapError TaskPort.errorToStringCustom


retrieveRemoteStorage : { token : String, userAddress : String } -> HypaethralBit -> Task String (Maybe Json.Decode.Value)
retrieveRemoteStorage { token, userAddress } bit =
    [ ( "fileName", fileName bit )
    , ( "token", Json.Encode.string token )
    , ( "userAddress", Json.Encode.string userAddress )
    ]
        |> TaskPort.call
            { function = "fromRemoteStorage"
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
    [ ( "fileName", fileName bit )
    , ( "data", data )
    , ( "token", Json.Encode.string accessToken )
    ]
        |> TaskPort.call
            { function = "toDropbox"
            , valueDecoder = TaskPort.ignoreValue
            , argsEncoder = Json.Encode.object
            }
        |> Task.mapError TaskPort.errorToStringCustom


saveIpfs : String -> HypaethralBit -> Json.Decode.Value -> Task String ()
saveIpfs apiOrigin bit data =
    [ ( "apiOrigin", Json.Encode.string apiOrigin )
    , ( "fileName", fileName bit )
    , ( "data", data )
    ]
        |> TaskPort.call
            { function = "toIpfs"
            , valueDecoder = TaskPort.ignoreValue
            , argsEncoder = Json.Encode.object
            }
        |> Task.mapError TaskPort.errorToStringCustom


saveRemoteStorage : { token : String, userAddress : String } -> HypaethralBit -> Json.Decode.Value -> Task String ()
saveRemoteStorage { token, userAddress } bit data =
    [ ( "fileName", fileName bit )
    , ( "data", data )
    , ( "token", Json.Encode.string token )
    , ( "userAddress", Json.Encode.string userAddress )
    ]
        |> TaskPort.call
            { function = "toRemoteStorage"
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



-- 🛠


fileName : HypaethralBit -> Json.Decode.Value
fileName =
    Json.Encode.string << hypaethralBitFileName
