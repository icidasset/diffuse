module Brain.Other.State exposing (..)

import Alien
import Brain.Common.State as Common
import Brain.Ports as Ports
import Brain.Task.Ports
import Brain.Types exposing (..)
import Dict
import Json.Decode as Json
import List.Extra as List
import Return exposing (return)
import Return.Ext as Return
import Sources exposing (Service(..))
import Sources.Encoding
import Sources.Refresh.AccessToken
import Time



-- 🔱


refreshedAccessToken : Json.Value -> Manager
refreshedAccessToken value model =
    case Json.decodeValue Sources.Refresh.AccessToken.portArgumentsDecoder value of
        Ok portArguments ->
            case portArguments.service of
                Google ->
                    model.hypaethralUserData.sources
                        |> List.find (.id >> (==) portArguments.sourceId)
                        |> Maybe.map
                            (\source ->
                                source.data
                                    |> Dict.insert "accessToken" portArguments.accessToken
                                    |> Dict.insert "expiresAt" (String.fromInt portArguments.expiresAt)
                                    |> (\newData -> { source | data = newData })
                            )
                        |> Maybe.map
                            (\source ->
                                source
                                    |> Sources.Encoding.encode
                                    |> Alien.broadcast Alien.UpdateSourceData
                                    |> Ports.toUI
                            )
                        |> Maybe.withDefault Cmd.none
                        |> return model

                _ ->
                    Return.singleton model

        Err err ->
            Common.reportUI Alien.ToCache (Json.errorToString err) model


setCurrentTime : Time.Posix -> Manager
setCurrentTime time model =
    Return.singleton { model | currentTime = time }


{-| Save alien data to cache.
-}
toCache : Json.Value -> Manager
toCache data =
    case Json.decodeValue Alien.hostDecoder data of
        Ok alienEvent ->
            case Alien.tagFromString alienEvent.tag of
                Just tag ->
                    alienEvent.data
                        |> Brain.Task.Ports.toCache tag
                        |> Common.attemptPortTask (always Bypass)
                        |> Return.communicate

                Nothing ->
                    Common.reportUI Alien.ToCache "Failed to decode alien tag"

        Err err ->
            err
                |> Json.errorToString
                |> Common.reportUI Alien.ToCache
