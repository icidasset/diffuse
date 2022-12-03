module Syncing exposing (..)

import Json.Decode as Decode
import Json.Encode as Json
import Maybe.Extra as Maybe
import Task exposing (Task)
import Task.Extra as Task
import Time
import User.Layer as User exposing (..)


{-| Syncs all hypaethral data.
-}
task :
    Task String a
    ->
        { localData : HypaethralData
        , saveLocal : HypaethralBit -> Decode.Value -> Task String ()
        }
    ->
        { retrieve : HypaethralBit -> Task String (Maybe Decode.Value)
        , save : HypaethralBit -> Decode.Value -> Task String ()
        }
    -> Task String HypaethralData
task initialTask { localData, saveLocal } { retrieve, save } =
    -- 1. Check if any existing data is present on the service to sync with.
    -- 2. If not, copy over all current data (in memory) to that service.
    --    If so: 👇
    -- 3. If no data is present locally then just load the remote data (ie. service data)
    --    No data = no sources, favourites & playlists
    --    If so: 👇
    -- 4. Compare modifiedAt timestamps
    --    (if no remote timestamp is available try to calculate it based on the data, progress → favourites → playlists → tracks → sources)
    --    If remote is newer: Load remote data
    --    Otherwise: 👇
    -- 5. Load remote data and run merge function for each type of data (sources, tracks, etc.)
    -- 6. Store merged data into memory
    -- 7. Overwrite remote data
    --
    -- 🏝️ LOCAL
    -- 🛰️ REMOTE
    let
        noLocalData =
            List.isEmpty localData.sources
                && List.isEmpty localData.favourites
                && List.isEmpty localData.playlists

        pushLocalToRemote { return } =
            localData
                |> User.encodedHypaethralDataList
                |> List.map (\( bit, data ) -> save bit data)
                |> Task.sequence
                |> Task.map (\_ -> return)
    in
    initialTask
        |> Task.andThen
            (\_ ->
                User.retrieveHypaethralData retrieve
            )
        |> Task.andThen
            (\list ->
                let
                    hasExistingData =
                        List.any (Tuple.second >> Maybe.isJust) list
                in
                if hasExistingData then
                    -- 🛰️
                    Task.succeed list

                else
                    -- 🏝️ → 🛰️
                    pushLocalToRemote { return = list }
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
                        if Time.posixToMillis remoteModifiedAt >= Time.posixToMillis localModifiedAt then
                            -- 🛰️
                            Task.succeed remoteData

                        else
                            -- 🏝️
                            pushLocalToRemote { return = localData }

                    ( Just _, Nothing ) ->
                        -- 🛰️
                        Task.succeed remoteData

                    ( Nothing, Just _ ) ->
                        -- 🏝️
                        pushLocalToRemote { return = localData }

                    _ ->
                        if noLocalData then
                            -- 🛰️
                            Task.succeed remoteData

                        else
                            -- 🏝️
                            Task.succeed localData
            )
        |> Task.andThen
            (\data ->
                data
                    |> User.saveHypaethralData saveLocal
                    |> Task.map (\_ -> data)
            )
