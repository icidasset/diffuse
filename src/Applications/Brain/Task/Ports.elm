module Brain.Task.Ports exposing (..)

import Alien
import Json.Decode
import Json.Encode
import TaskPort



-- CACHE


fromCache : Alien.Tag -> Json.Decode.Decoder value -> TaskPort.Task (Maybe value)
fromCache tag decoder =
    TaskPort.call
        { function = "fromCache"
        , valueDecoder = Json.Decode.maybe decoder
        , argsEncoder = Json.Encode.string
        }
        (Alien.tagToString tag)


fromCacheWithSuffix : Alien.Tag -> String -> Json.Decode.Decoder value -> TaskPort.Task (Maybe value)
fromCacheWithSuffix tag suffix decoder =
    TaskPort.call
        { function = "fromCache"
        , valueDecoder = Json.Decode.maybe decoder
        , argsEncoder = Json.Encode.string
        }
        (Alien.tagToString tag ++ "_" ++ suffix)


removeCache : Alien.Tag -> TaskPort.Task ()
removeCache tag =
    TaskPort.call
        { function = "removeCache"
        , valueDecoder = TaskPort.ignoreValue
        , argsEncoder = Json.Encode.string
        }
        (Alien.tagToString tag)


toCache : Alien.Tag -> Json.Encode.Value -> TaskPort.Task ()
toCache tag =
    let
        key =
            Alien.tagToString tag
    in
    TaskPort.call
        { function = "toCache"
        , valueDecoder = TaskPort.ignoreValue
        , argsEncoder = \v -> Json.Encode.object [ ( "key", Json.Encode.string key ), ( "value", v ) ]
        }


toCacheWithSuffix : Alien.Tag -> String -> Json.Encode.Value -> TaskPort.Task ()
toCacheWithSuffix tag suffix =
    let
        key =
            Alien.tagToString tag ++ "_" ++ suffix
    in
    TaskPort.call
        { function = "toCache"
        , valueDecoder = TaskPort.ignoreValue
        , argsEncoder = \v -> Json.Encode.object [ ( "key", Json.Encode.string key ), ( "value", v ) ]
        }



-- CRYPTO


fabricateSecretKey : String -> TaskPort.Task ()
fabricateSecretKey =
    TaskPort.call
        { function = "fabricateSecretKey"
        , valueDecoder = Json.Decode.succeed ()
        , argsEncoder = Json.Encode.string
        }
