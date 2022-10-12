module Brain.User.Hypaethral exposing (..)

import Alien
import Brain.Task.Ports
import Json.Decode
import Json.Encode
import Task
import User.Layer as User exposing (..)



-- 🔱


retrieveAll retrievalFn =
    hypaethralBit.list
        |> List.map
            (\( _, bit ) ->
                bit
                    |> retrievalFn
                    |> Task.map
                        (\value ->
                            ( bit
                            , Maybe.withDefault Json.Encode.null value
                            , BaggageClaimed
                            )
                        )
            )
        |> Task.sequence
        |> Task.map (putHypaethralJsonBitsTogether >> User.decodeHypaethralData)



-- METHODS


retrieveLocal bit =
    Brain.Task.Ports.fromCacheWithSuffix
        Alien.AuthAnonymous
        (hypaethralBitFileName bit)
        Json.Decode.value
