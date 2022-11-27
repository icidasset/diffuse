module Brain.User.Hypaethral exposing (..)

import Alien
import Brain.Task.Ports
import Json.Decode
import Json.Encode
import Task exposing (Task)
import TaskPort
import User.Layer as User exposing (..)



-- 🔱


retrieveAll : (HypaethralBit -> Task x (Maybe Json.Decode.Value)) -> Task x (List ( HypaethralBit, Maybe Json.Encode.Value ))
retrieveAll retrievalFn =
    hypaethralBit.list
        |> List.map
            (\( _, bit ) ->
                bit
                    |> retrievalFn
                    |> Task.map (\value -> ( bit, value ))
            )
        |> Task.sequence



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
        Alien.AuthAnonymous
        (hypaethralBitFileName bit)
        Json.Decode.value



-- STORAGE


toDropbox : String -> HypaethralBit -> Json.Decode.Value -> TaskPort.Task ()
toDropbox accessToken bit data =
    TaskPort.call
        { function = "toDropbox"
        , valueDecoder = TaskPort.ignoreValue
        , argsEncoder = Json.Encode.object
        }
        [ ( "fileName", Json.Encode.string (hypaethralBitFileName bit) )
        , ( "data", data )
        , ( "token", Json.Encode.string accessToken )
        ]
