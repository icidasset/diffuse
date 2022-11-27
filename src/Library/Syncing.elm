module Syncing exposing (..)

import Json.Decode as Decode
import Json.Encode as Json
import Maybe.Extra as Maybe
import Task exposing (Task)
import Task.Extra as Task
import TaskPort
import TaskPort.Extra as TaskPort
import Time
import User.Layer as User exposing (..)


task :
    Task String a
    ->
        { localData : HypaethralData
        , saveLocal : HypaethralBit -> Decode.Value -> TaskPort.Task ()
        }
    ->
        { retrieve : HypaethralBit -> TaskPort.Task (Maybe Decode.Value)
        , save : HypaethralBit -> Decode.Value -> TaskPort.Task ()
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
    in
    initialTask
        |> Task.andThen
            (\_ ->
                retrieve
                    |> User.retrieveHypaethralData
                    |> Task.mapError TaskPort.errorToString
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
                    -- 🏝️ → 🛰️ / Push to remote
                    localData
                        |> User.encodedHypaethralDataList
                        |> List.map (\( bit, data ) -> save bit data)
                        |> Task.sequence
                        |> Task.mapError TaskPort.errorToString
                        |> Task.map (\_ -> list)
            )
        |> Task.andThen
            (\list ->
                -- Decode
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
                case Debug.log "" ( remoteData.modifiedAt, localData.modifiedAt ) of
                    ( Just remoteModifiedAt, Just localModifiedAt ) ->
                        let
                            _ =
                                Debug.log "isNewer" (Time.posixToMillis remoteModifiedAt > Time.posixToMillis localModifiedAt)
                        in
                        if Time.posixToMillis remoteModifiedAt > Time.posixToMillis localModifiedAt then
                            -- 🛰️
                            Task.succeed remoteData

                        else
                            -- 🏝️ -> 🛰️
                            localData
                                |> User.encodedHypaethralDataList
                                |> List.map (\( bit, data ) -> save bit data)
                                |> Task.sequence
                                |> Task.mapError TaskPort.errorToString
                                |> Task.map (\_ -> localData)

                    -- TODO: Do we need to match for (Just, Nothing) or (Nothing, Just)?
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
                    |> Task.mapError TaskPort.errorToString
                    |> Task.map (\_ -> data)
            )
