module Syncing exposing (LocalConfig, RemoteConfig, task)

import Json.Decode as Decode
import Json.Encode as Json
import Maybe.Extra as Maybe
import Task exposing (Task)
import Task.Extra as Task
import Time
import Time.Ext as Time
import User.Layer as User exposing (..)



-- 🌳


type alias LocalConfig =
    { localData : HypaethralData
    , saveLocal : HypaethralBit -> Decode.Value -> Task String ()
    }


type alias RemoteConfig =
    { retrieve : HypaethralBit -> Task String (Maybe Decode.Value)
    , save : HypaethralBit -> Decode.Value -> Task String ()
    }



-- 🛠


{-| Syncs all hypaethral data.

Returns `Nothing` if the local data is preferred.

🏝️ LOCAL
🛰️ REMOTE

1.  Try to pull remote `modified.json` timestamp
    a. If newer, continue (#2)
    b. If same, do nothing
    c. If older, or not present, prefer local data 🏝️ (stop & push)
2.  Try to download all remote data
    a. If any remote data, continue (#3)
    b. If none, prefer local data 🏝️ (stop & push)
3.  Decode remote data and compare timestamps
    a. If newer, use remote data 🛰️
    b. If same, do nothing
    c. If older, prefer local data 🏝️ (stop & push)
    d. If no timestamps, if local data, prefer local 🏝️ (stop & push), otherwise remote 🛰️

-}
task :
    Task String a
    -> LocalConfig
    -> RemoteConfig
    -> Task String (Maybe HypaethralData)
task initialTask localConfig remoteConfig =
    initialTask
        |> Task.andThen
            (\_ ->
                remoteConfig.retrieve ModifiedAt
            )
        |> Task.andThen
            (\maybeModifiedAt ->
                let
                    maybeRemoteModifiedAt =
                        Maybe.andThen
                            (Decode.decodeValue Time.decoder >> Result.toMaybe)
                            maybeModifiedAt
                in
                case ( maybeRemoteModifiedAt, localConfig.localData.modifiedAt ) of
                    ( Just remoteModifiedAt, Just localModifiedAt ) ->
                        if Time.posixToMillis remoteModifiedAt == Time.posixToMillis localModifiedAt then
                            -- 🏝️
                            Task.succeed Nothing

                        else if Time.posixToMillis remoteModifiedAt > Time.posixToMillis localModifiedAt then
                            -- 🛰️
                            fetchRemote localConfig remoteConfig

                        else
                            -- 🏝️ → 🛰️
                            pushLocalToRemote localConfig remoteConfig { return = Nothing }

                    ( Just _, Nothing ) ->
                        -- 🛰️
                        fetchRemote localConfig remoteConfig

                    ( Nothing, _ ) ->
                        -- 🛰️
                        fetchRemote localConfig remoteConfig
            )


fetchRemote :
    LocalConfig
    -> RemoteConfig
    -> Task String (Maybe HypaethralData)
fetchRemote localConfig remoteConfig =
    let
        { localData, saveLocal } =
            localConfig

        { retrieve } =
            remoteConfig

        saveLocally data =
            data
                |> User.saveHypaethralData saveLocal
                |> Task.map (\_ -> Just data)

        noLocalData =
            List.isEmpty localData.sources
                && List.isEmpty localData.favourites
                && List.isEmpty localData.playlists
    in
    retrieve
        |> User.retrieveHypaethralData
        |> Task.andThen
            (\list ->
                let
                    remoteHasExistingData =
                        List.any (Tuple.second >> Maybe.isJust) list
                in
                if remoteHasExistingData then
                    -- 🛰️
                    Task.succeed list

                else
                    -- 🏝️ → 🛰️
                    pushLocalToRemote localConfig remoteConfig { return = list }
            )
        |> Task.andThen
            (\list ->
                -- Decode remote
                list
                    |> List.map (\( a, b ) -> ( hypaethralBitKey a, Maybe.withDefault Json.null b ))
                    |> Json.object
                    |> User.decodeHypaethralData
                    |> Task.fromResult
                    |> Task.mapError Decode.errorToString
            )
        |> Task.andThen
            (\remoteData ->
                -- Compare modifiedAt timestamps
                case ( remoteData.modifiedAt, localData.modifiedAt ) of
                    ( Just remoteModifiedAt, Just localModifiedAt ) ->
                        if Time.posixToMillis remoteModifiedAt == Time.posixToMillis localModifiedAt then
                            -- 🏝️
                            Task.succeed Nothing

                        else if Time.posixToMillis remoteModifiedAt > Time.posixToMillis localModifiedAt then
                            -- 🛰️
                            saveLocally remoteData

                        else
                            -- 🏝️ → 🛰️
                            pushLocalToRemote localConfig remoteConfig { return = Nothing }

                    ( Just _, Nothing ) ->
                        -- 🛰️
                        saveLocally remoteData

                    ( Nothing, Just _ ) ->
                        -- 🏝️ → 🛰️
                        pushLocalToRemote localConfig remoteConfig { return = Nothing }

                    _ ->
                        if noLocalData then
                            -- 🛰️
                            saveLocally remoteData

                        else
                            -- 🏝️
                            Task.succeed Nothing
            )



-- ㊙️


pushLocalToRemote : LocalConfig -> RemoteConfig -> { return : a } -> Task String a
pushLocalToRemote localConfig remoteConfig { return } =
    localConfig.localData
        |> User.encodedHypaethralDataList
        |> List.map (\( bit, data ) -> remoteConfig.save bit data)
        |> Task.sequence
        |> Task.map (\_ -> return)
