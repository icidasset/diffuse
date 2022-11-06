module Brain.User.Hypaethral exposing (..)

import Alien
import Brain.Task.Ports
import Json.Decode
import Json.Encode
import Task exposing (Task)
import TaskPort
import Time
import User.Layer as User exposing (..)



-- 🔱


retrieveAll : (HypaethralBit -> Task x (Maybe Json.Decode.Value)) -> Task x Json.Decode.Value
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
        |> Task.map putHypaethralJsonBitsTogether



-- RETRIEVAL


retrieveDropbox : String -> HypaethralBit -> TaskPort.Task (Maybe Json.Decode.Value)
retrieveDropbox accessToken bit =
    Brain.Task.Ports.requestDropbox
        { file = hypaethralBitFileName bit
        , token = accessToken
        }


retrieveLocal : HypaethralBit -> TaskPort.Task (Maybe Json.Decode.Value)
retrieveLocal bit =
    Brain.Task.Ports.fromCacheWithSuffix
        Alien.AuthAnonymous
        (hypaethralBitFileName bit)
        Json.Decode.value



-- DROPBOX


isDropboxTokenExpired : { currentTime : Time.Posix, expiresAt : Int } -> Bool
isDropboxTokenExpired { currentTime, expiresAt } =
    let
        currentTimeInSeconds =
            Time.posixToMillis currentTime // 1000

        currentTimeWithOffset =
            -- We add 60 seconds here because we only get the current time every minute,
            -- so there's always the chance the "current time" is 1-60 seconds behind.
            currentTimeInSeconds + 60
    in
    -- If the access token is expired
    currentTimeWithOffset >= expiresAt
