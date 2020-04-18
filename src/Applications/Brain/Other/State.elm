module Brain.Other.State exposing (..)

import Alien
import Brain.Common.State as Common
import Brain.Ports as Ports
import Brain.Types exposing (..)
import Json.Decode as Json
import Return
import Return.Ext as Return
import Time



-- 🔱


redirectToBlockstackSignIn : Manager
redirectToBlockstackSignIn =
    Return.communicate (Ports.redirectToBlockstackSignIn ())


setCurrentTime : Time.Posix -> Manager
setCurrentTime time model =
    Return.singleton { model | currentTime = time }


toCache : Json.Value -> Manager
toCache data =
    case Json.decodeValue Alien.hostDecoder data of
        Ok alienEvent ->
            alienEvent
                |> Ports.toCache
                |> Return.communicate

        Err err ->
            err
                |> Json.errorToString
                |> Common.reportUI Alien.ToCache
