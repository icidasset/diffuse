module Brain.User.State exposing (..)

import Alien
import Brain.Common.State as Common
import Brain.Ports as Ports
import Brain.Task.Ports
import Brain.Types as Brain exposing (..)
import Brain.User.Hypaethral as Hypaethral
import Brain.User.Types as User exposing (..)
import Debouncer.Basic as Debouncer
import EverySet
import Json.Decode as Decode
import Json.Encode as Json
import Playlists.Encoding as Playlists
import Return exposing (andThen, return)
import Return.Ext as Return
import Settings
import Sources.Encoding as Sources
import Syncing
import Syncing.Services.Dropbox.Token
import Task exposing (Task)
import TaskPort.Extra as TaskPort
import Time
import Tracks exposing (Track)
import Tracks.Encoding as Tracks
import Url exposing (Url)
import Url.Ext as Url
import User.Layer as User exposing (..)
import User.Layer.Methods.Dropbox as Dropbox



-- 🌳


initialCommand : Url -> Cmd Brain.Msg
initialCommand uiUrl =
    case Url.action uiUrl of
        _ ->
            Cmd.batch
                [ loadEnclosedData
                , loadLocalHypaethralData
                    { initialUrl = uiUrl
                    , methodTask = loadSyncMethod
                    }
                ]


{-| Loads the "enclosed" data from cache and sends it to the UI.
-}
loadEnclosedData : Cmd Brain.Msg
loadEnclosedData =
    Decode.value
        |> Brain.Task.Ports.fromCache Alien.EnclosedData
        |> Task.map (Maybe.withDefault Json.null)
        |> Common.attemptPortTask (Common.giveUICmdMsg Alien.LoadEnclosedUserData)


{-| Loads the "sync method".
-}
loadSyncMethod : Task String (Maybe Method)
loadSyncMethod =
    Decode.value
        |> Brain.Task.Ports.fromCache Alien.SyncMethod
        |> Task.mapError TaskPort.errorToStringCustom
        |> Task.map (Maybe.andThen decodeMethod)


{-| Loads the "sync method" and "hypaethral" data,
see `Commence` Msg what happens next.
-}
loadLocalHypaethralData : { initialUrl : Url, methodTask : Task String (Maybe Method) } -> Cmd Brain.Msg
loadLocalHypaethralData { initialUrl, methodTask } =
    methodTask
        |> Task.andThen
            (\maybeMethod ->
                Hypaethral.retrieveLocal
                    |> User.retrieveHypaethralData
                    |> Task.map
                        (\bits ->
                            bits
                                |> List.map (\( a, b ) -> ( hypaethralBitKey a, Maybe.withDefault Json.null b ))
                                |> Json.object
                        )
                    |> Task.map (Tuple.pair maybeMethod)
            )
        |> Common.attemptTask
            (\( maybeMethod, hypaethralJson ) ->
                hypaethralJson
                    |> User.decodeHypaethralData
                    |> Result.map
                        (\hypaethralData ->
                            Commence
                                maybeMethod
                                initialUrl
                                ( hypaethralJson
                                , hypaethralData
                                )
                        )
                    |> Result.mapError Decode.errorToString
                    |> Common.reportErrorToUI UserMsg
            )



-- 📣


update : User.Msg -> Manager
update msg =
    case msg of
        Commence a b c ->
            commence a b c

        SetSyncMethod a ->
            setSyncMethod a

        Sync ->
            sync { initialTask = Nothing }

        UnsetSyncMethod ->
            unsetSyncMethod

        -----------------------------------------
        -- x. Data
        -----------------------------------------
        RetrieveEnclosedData ->
            retrieveEnclosedData

        EnclosedDataRetrieved a ->
            enclosedDataRetrieved a

        SaveEnclosedData a ->
            saveEnclosedData a

        -----------------------------------------
        -- y. Data
        -----------------------------------------
        --   The hypaethral user data is received in pieces,
        --   pieces which are "cached" here in the web worker.
        --
        --   The reasons for this are:
        --   1. Lesser performance penalty on the UI when saving data
        --      (ie. this avoids having to encode/decode everything each time)
        --   2. The data can be used in the web worker (brain) as well.
        --      (eg. for track-search index)
        --
        SaveFavourites a ->
            saveFavourites a

        SavePlaylists a ->
            savePlaylists a

        SaveProgress a ->
            saveProgress a

        SaveSettings a ->
            saveSettings a

        SaveSources a ->
            saveSources a

        SaveTracks a ->
            saveTracks a

        -----------------------------------------
        -- z. Data
        -----------------------------------------
        FinishedSyncing ->
            finishedSyncing

        GotHypaethralData a ->
            gotHypaethralData a

        SaveHypaethralDataBits a ->
            saveHypaethralDataBits a

        SaveHypaethralDataSlowly a ->
            saveHypaethralDataSlowly a

        -----------------------------------------
        -- z. Secret Key
        -----------------------------------------
        RemoveEncryptionKey ->
            removeEncryptionKey

        UpdateEncryptionKey a ->
            updateEncryptionKey a

        -----------------------------------------
        -- 📭 Other
        -----------------------------------------
        RefreshedDropboxTokens a b c ->
            refreshedDropboxTokens a b c



-- 🔱


commence : Maybe Method -> Url -> ( Json.Value, HypaethralData ) -> Manager
commence maybeMethod initialUrl ( hypaethralJson, hypaethralData ) model =
    -- 🚀
    -- Initiated from `initialCommand`.
    -- Loaded the used-sync method and the local hypaethral data.
    { model | userSyncMethod = maybeMethod }
        |> sendHypaethralDataToUI hypaethralJson hypaethralData
        |> andThen
            (case Url.action initialUrl of
                _ ->
                    sync { initialTask = Nothing }
            )


setSyncMethod : Json.Value -> Manager
setSyncMethod json model =
    -- 🐤
    -- Set & store method,
    -- and retrieve data.
    let
        decoder =
            Decode.map2
                (\a b -> ( a, b ))
                (Decode.field "method" <| Decode.map methodFromString Decode.string)
                (Decode.field "passphrase" <| Decode.maybe Decode.string)
    in
    case Decode.decodeValue decoder json of
        Ok ( Just method, Just passphrase ) ->
            let
                initialTask =
                    passphrase
                        |> Brain.Task.Ports.fabricateSecretKey
                        |> Task.mapError TaskPort.errorToStringCustom
            in
            { model | userSyncMethod = Just method }
                |> sync { initialTask = Just initialTask }
                |> andThen (saveMethod method)

        Ok ( Just method, Nothing ) ->
            { model | userSyncMethod = Just method }
                |> sync { initialTask = Nothing }
                |> andThen (saveMethod method)

        Ok ( Nothing, _ ) ->
            Return.singleton { model | userSyncMethod = Nothing }

        Err _ ->
            Return.singleton model


sync : { initialTask : Maybe (Task.Task String ()) } -> Manager
sync { initialTask } model =
    model
        |> syncCommand (Maybe.withDefault (Task.succeed ()) initialTask)
        |> return model
        |> andThen
            (case model.userSyncMethod of
                Just method ->
                    Common.giveUI Alien.StartedSyncing (encodeMethod method)

                Nothing ->
                    Return.singleton
            )


syncCommand : Task.Task String a -> Model -> Cmd Brain.Msg
syncCommand initialTask model =
    let
        localData =
            model.hypaethralUserData

        attemptSync args =
            args
                |> Syncing.task
                    initialTask
                    { localData = localData
                    , saveLocal = Hypaethral.saveLocal
                    }
                |> Common.attemptTask
                    (\maybe ->
                        case maybe of
                            Just data ->
                                UserMsg (GotHypaethralData data)

                            Nothing ->
                                UserMsg FinishedSyncing
                    )
    in
    case model.userSyncMethod of
        Just (Dropbox { accessToken, expiresAt, refreshToken }) ->
            if
                Syncing.Services.Dropbox.Token.isExpired
                    { currentTime = model.currentTime
                    , expiresAt = expiresAt
                    }
            then
                refreshDropboxTokens
                    model.currentTime
                    Sync
                    initialTask
                    refreshToken

            else
                attemptSync
                    { retrieve = Hypaethral.retrieveDropbox accessToken
                    , save = Hypaethral.saveDropbox accessToken
                    }

        Just (Ipfs { apiOrigin }) ->
            attemptSync
                { retrieve = Hypaethral.retrieveIpfs apiOrigin
                , save = Hypaethral.saveIpfs apiOrigin
                }

        Just (RemoteStorage args) ->
            attemptSync
                { retrieve = Hypaethral.retrieveRemoteStorage args
                , save = Hypaethral.saveRemoteStorage args
                }

        Nothing ->
            Cmd.none


unsetSyncMethod : Manager
unsetSyncMethod model =
    -- 💀
    -- Unset & remove stored method.
    [ Common.attemptPortTask (always Brain.Bypass) (Brain.Task.Ports.removeCache Alien.SyncMethod)
    , Common.attemptPortTask (always Brain.Bypass) (Brain.Task.Ports.removeCache Alien.SecretKey)

    --
    , case model.userSyncMethod of
        Just (Dropbox _) ->
            Cmd.none

        Just (Ipfs _) ->
            Cmd.none

        Just (RemoteStorage _) ->
            Ports.deconstructRemoteStorage ()

        Nothing ->
            Cmd.none
    ]
        |> Cmd.batch
        |> return { model | userSyncMethod = Nothing }



-- 🔱  ░░  DATA - ENCLOSED


enclosedDataRetrieved : Json.Value -> Manager
enclosedDataRetrieved json =
    Common.giveUI Alien.LoadEnclosedUserData json


retrieveEnclosedData : Manager
retrieveEnclosedData =
    Decode.value
        |> Brain.Task.Ports.fromCache Alien.EnclosedData
        |> Common.attemptPortTask
            (\maybe ->
                case maybe of
                    Just json ->
                        Brain.UserMsg (EnclosedDataRetrieved json)

                    Nothing ->
                        Brain.Bypass
            )
        |> Return.communicate


saveEnclosedData : Json.Value -> Manager
saveEnclosedData json =
    json
        |> Brain.Task.Ports.toCache Alien.EnclosedData
        |> Common.attemptPortTask (always Brain.Bypass)
        |> Return.communicate



-- 🔱  ░░  DATA - HYPAETHRAL


finishedSyncing : Manager
finishedSyncing model =
    case model.userSyncMethod of
        Just userSyncMethod ->
            Common.giveUI Alien.SyncMethod (encodeMethod userSyncMethod) model

        Nothing ->
            Return.singleton model


gotHypaethralData : HypaethralData -> Manager
gotHypaethralData hypaethralData model =
    model
        |> sendHypaethralDataToUI (User.encodeHypaethralData hypaethralData) hypaethralData
        |> andThen finishedSyncing


saveAllHypaethralDataTask : HypaethralData -> Method -> Task String ()
saveAllHypaethralDataTask userData method =
    let
        save =
            saveHypaethralDataBitsTask (ModifiedAt :: User.allHypaethralBits) userData
    in
    case method of
        Dropbox { accessToken } ->
            save (Hypaethral.saveDropbox accessToken)

        Ipfs { apiOrigin } ->
            save (Hypaethral.saveIpfs apiOrigin)

        RemoteStorage a ->
            save (Hypaethral.saveRemoteStorage a)


saveHypaethralDataBitsTask : List HypaethralBit -> HypaethralData -> (HypaethralBit -> Json.Value -> Task String ()) -> Task String ()
saveHypaethralDataBitsTask bits userData saveFn =
    [ --------
      -- LOCAL
      --------
      List.map
        (\bit ->
            Hypaethral.saveLocal bit (encodeHypaethralBit bit userData)
        )
        bits
    , ---------
      -- REMOTE
      ---------
      List.map
        (\bit ->
            saveFn bit (encodeHypaethralBit bit userData)
        )
        bits
    ]
        |> List.concat
        |> List.foldl
            (\nextTask -> Task.andThen (\_ -> nextTask))
            (Task.succeed ())


{-| Save different parts of hypaethral data,
one part at a time.
-}
saveHypaethralDataBits : List HypaethralBit -> Manager
saveHypaethralDataBits bitsWithoutModifiedAt model =
    let
        bits =
            ModifiedAt :: bitsWithoutModifiedAt

        userData =
            model.hypaethralUserData

        updatedUserData =
            { userData | modifiedAt = Just model.currentTime }

        updatedModel =
            { model | hypaethralUserData = updatedUserData }

        save saveFn =
            Time.now
                |> Task.andThen
                    (\currentTime ->
                        saveHypaethralDataBitsTask
                            bits
                            { updatedUserData | modifiedAt = Just currentTime }
                            saveFn
                    )
                |> Common.attemptTask (always Brain.Bypass)
                |> return updatedModel
    in
    case model.userSyncMethod of
        Just (Dropbox { accessToken, expiresAt, refreshToken }) ->
            if
                Syncing.Services.Dropbox.Token.isExpired
                    { currentTime = model.currentTime
                    , expiresAt = expiresAt
                    }
            then
                refreshToken
                    |> refreshDropboxTokens
                        model.currentTime
                        (SaveHypaethralDataBits bits)
                        (Task.succeed ())
                    |> return model

            else
                save (Hypaethral.saveDropbox accessToken)

        Just (Ipfs { apiOrigin }) ->
            save (Hypaethral.saveIpfs apiOrigin)

        Just (RemoteStorage args) ->
            save (Hypaethral.saveRemoteStorage args)

        Nothing ->
            -- Only save locally
            save (\_ _ -> Task.succeed ())


saveHypaethralDataBitWithDebounce : HypaethralBit -> Manager
saveHypaethralDataBitWithDebounce bit =
    bit
        |> Debouncer.provideInput
        |> saveHypaethralDataSlowly


saveHypaethralDataSlowly : Debouncer.Msg HypaethralBit -> Manager
saveHypaethralDataSlowly debouncerMsg model =
    let
        ( m, c, e ) =
            Debouncer.update debouncerMsg model.hypaethralDebouncer

        bits =
            e
                |> Maybe.withDefault []
                |> EverySet.fromList
                |> EverySet.toList
    in
    c
        |> Cmd.map (SaveHypaethralDataSlowly >> UserMsg)
        |> return { model | hypaethralDebouncer = m }
        |> (if not (List.isEmpty bits) then
                andThen (saveHypaethralDataBits bits)

            else
                identity
           )


sendHypaethralDataToUI : Json.Value -> HypaethralData -> Manager
sendHypaethralDataToUI encodedData decodedData model =
    [ encodedData
        |> Alien.broadcast Alien.LoadHypaethralUserData
        |> Ports.toUI

    --
    , decodedData.tracks
        |> Json.list Tracks.encodeTrack
        |> Ports.updateSearchIndex
    ]
        |> Cmd.batch
        |> return { model | hypaethralUserData = decodedData }



-- 🔱  ░░  DATA - HYPAETHRAL BITS


saveFavourites : Json.Value -> Manager
saveFavourites value model =
    value
        |> Decode.decodeValue (Decode.list Tracks.favouriteDecoder)
        |> Result.withDefault model.hypaethralUserData.favourites
        |> hypaethralLenses.setFavourites model
        |> saveHypaethralDataBitWithDebounce Favourites


savePlaylists : Json.Value -> Manager
savePlaylists value model =
    value
        |> Decode.decodeValue (Decode.list Playlists.decoder)
        |> Result.withDefault model.hypaethralUserData.playlists
        |> hypaethralLenses.setPlaylists model
        |> saveHypaethralDataBitWithDebounce Playlists


saveProgress : Json.Value -> Manager
saveProgress value model =
    value
        |> Decode.decodeValue (Decode.dict Decode.float)
        |> Result.withDefault model.hypaethralUserData.progress
        |> hypaethralLenses.setProgress model
        |> saveHypaethralDataBitWithDebounce Progress


saveSettings : Json.Value -> Manager
saveSettings value model =
    value
        |> Decode.decodeValue (Decode.map Just Settings.decoder)
        |> Result.withDefault model.hypaethralUserData.settings
        |> hypaethralLenses.setSettings model
        |> saveHypaethralDataBitWithDebounce Settings


saveSources : Json.Value -> Manager
saveSources value model =
    value
        |> Decode.decodeValue (Decode.list Sources.decoder)
        |> Result.withDefault model.hypaethralUserData.sources
        |> hypaethralLenses.setSources model
        |> saveHypaethralDataBitWithDebounce Sources


saveTracks : Json.Value -> Manager
saveTracks value model =
    saveTracksAndUpdateSearchIndex
        (value
            |> Decode.decodeValue (Decode.list Tracks.trackDecoder)
            |> Result.withDefault model.hypaethralUserData.tracks
        )
        model


saveTracksAndUpdateSearchIndex : List Track -> Manager
saveTracksAndUpdateSearchIndex tracks model =
    tracks
        -- Store in model
        |> hypaethralLenses.setTracks model
        -- Update search index
        |> Return.communicate
            (tracks
                |> Json.list Tracks.encodeTrack
                |> Ports.updateSearchIndex
            )
        -- Save with delay
        |> andThen (saveHypaethralDataBitWithDebounce Tracks)



-- 🔱  ░░  DATA - HYPAETHRAL LENSES


hypaethralLenses =
    { setFavourites = makeHypaethralLens (\h f -> { h | favourites = f })
    , setPlaylists = makeHypaethralLens (\h p -> { h | playlists = p })
    , setProgress = makeHypaethralLens (\h p -> { h | progress = p })
    , setSettings = makeHypaethralLens (\h s -> { h | settings = s })
    , setSources = makeHypaethralLens (\h s -> { h | sources = s })
    , setTracks = makeHypaethralLens (\h t -> { h | tracks = t })
    }


makeHypaethralLens : (HypaethralData -> a -> HypaethralData) -> Model -> a -> Model
makeHypaethralLens setter model value =
    { model | hypaethralUserData = setter model.hypaethralUserData value }



-- 🔱  ░░  METHOD


saveMethod : Method -> Manager
saveMethod method model =
    method
        |> encodeMethod
        |> Brain.Task.Ports.toCache Alien.SyncMethod
        |> Common.attemptPortTask (always Brain.Bypass)
        |> return { model | userSyncMethod = Just method }



-- 🔱  ░░  SECRET KEY


removeEncryptionKey : Manager
removeEncryptionKey model =
    Alien.SecretKey
        |> Brain.Task.Ports.removeCache
        |> Task.mapError TaskPort.errorToStringCustom
        |> Task.andThen (\_ -> Time.now)
        |> Task.andThen
            (\currentTime ->
                case model.userSyncMethod of
                    Just method ->
                        let
                            data =
                                model.hypaethralUserData
                        in
                        saveAllHypaethralDataTask { data | modifiedAt = Just currentTime } method

                    Nothing ->
                        Task.succeed ()
            )
        |> Common.attemptTask (always Brain.Bypass)
        |> return model


updateEncryptionKey : Json.Value -> Manager
updateEncryptionKey json model =
    case Decode.decodeValue Decode.string json of
        Ok passphrase ->
            passphrase
                |> Brain.Task.Ports.fabricateSecretKey
                |> Task.mapError TaskPort.errorToStringCustom
                |> Task.andThen (\_ -> Time.now)
                |> Task.andThen
                    (\currentTime ->
                        case model.userSyncMethod of
                            Just method ->
                                let
                                    data =
                                        model.hypaethralUserData
                                in
                                saveAllHypaethralDataTask { data | modifiedAt = Just currentTime } method

                            Nothing ->
                                Task.succeed ()
                    )
                |> Common.attemptTask (always Brain.Bypass)
                |> return model

        Err _ ->
            Return.singleton model



-- 📭  ░░  OTHER


refreshDropboxTokens : Time.Posix -> User.Msg -> Task.Task String a -> String -> Cmd Brain.Msg
refreshDropboxTokens currentTime msg initialTask refreshToken =
    initialTask
        |> Task.andThen
            (\_ -> Dropbox.refreshAccessToken refreshToken)
        |> Task.attempt
            (\result ->
                case result of
                    Ok tokens ->
                        msg
                            |> RefreshedDropboxTokens
                                { currentTime = Time.posixToMillis currentTime // 1000
                                , refreshToken = refreshToken
                                }
                                tokens
                            |> UserMsg

                    Err err ->
                        Common.reportUICmdMsg Alien.ReportError err
            )


refreshedDropboxTokens :
    { currentTime : Int, refreshToken : String }
    -> Dropbox.Tokens
    -> User.Msg
    -> Manager
refreshedDropboxTokens { currentTime, refreshToken } tokens msg model =
    { accessToken = tokens.accessToken
    , expiresAt = currentTime + tokens.expiresIn
    , refreshToken = refreshToken
    }
        |> Dropbox
        |> (\m -> saveMethod m model)
        |> andThen (update msg)
