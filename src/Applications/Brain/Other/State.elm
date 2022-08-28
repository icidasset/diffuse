module Brain.Other.State exposing (..)

import Alien
import Brain.Common.State as Common
import Brain.Ports as Ports
import Brain.Types exposing (..)
import Json.Decode as Json
import Return
import Return.Ext as Return
import Sources exposing (Service(..))
import Sources.Refresh.AccessToken
import Time



-- 🔱


refreshedAccessToken : Json.Value -> Manager
refreshedAccessToken value model =
    case Json.decodeValue Sources.Refresh.AccessToken.portArgumentsDecoder value of
        Ok portArguments ->
            case portArguments.service of
                Google ->
                    -- TODO:
                    -- 1. Find source
                    -- 2. Add `access token` and `expires at` to source data
                    -- 3. Replace source in collection
                    -- 4. Save user data
                    Return.singleton model

                _ ->
                    Return.singleton model

        Err err ->
            Common.reportUI Alien.ToCache (Json.errorToString err) model


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
