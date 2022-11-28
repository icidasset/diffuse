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
import List.Zipper as Zipper
import Playlists.Encoding as Playlists
import Return exposing (andThen, return)
import Return.Ext as Return
import Settings
import Sources.Encoding as Sources
import Syncing
import Syncing.Services.Dropbox.Token
import Task
import Task.Extra as Task exposing (do)
import TaskPort.Extra as TaskPort
import Time
import Tracks exposing (Track)
import Tracks.Encoding as Tracks
import Tuple3
import UI.Authentication.Types exposing (State(..))
import Url exposing (Url)
import Url.Ext as Url
import User.Layer as User exposing (..)
import User.Layer.Methods.Dropbox as Dropbox
import User.Layer.Methods.Fission as Fission
import Webnative



-- 🌳


initialCommand : Url -> Cmd Brain.Msg
initialCommand uiUrl =
    case Url.action uiUrl of
        [ "authenticate", "fission" ] ->
            Cmd.none

        _ ->
            Cmd.batch
                [ loadEnclosedData
                , loadSyncMethodAndLocalHypaethralData
                ]


{-| Loads the "enclosed" data from cache and sends it to the UI.
-}
loadEnclosedData : Cmd Brain.Msg
loadEnclosedData =
    Decode.value
        |> Brain.Task.Ports.fromCache Alien.AuthEnclosedData
        |> Task.map (Maybe.withDefault Json.null)
        |> Common.attemptPortTask (Common.giveUICmdMsg Alien.LoadEnclosedUserData)


{-| Loads the "sync method" and "hypaethral" data,
see `Commence` Msg what happens next.
-}
loadSyncMethodAndLocalHypaethralData : Cmd Brain.Msg
loadSyncMethodAndLocalHypaethralData =
    Decode.value
        |> Brain.Task.Ports.fromCache Alien.SyncMethod
        |> Task.andThen
            (\json ->
                let
                    maybeMethod =
                        Maybe.andThen decodeMethod json
                in
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
        |> Common.attemptPortTask
            (\( maybeMethod, hypaethralJson ) ->
                hypaethralJson
                    |> User.decodeHypaethralData
                    |> Result.map
                        (\hypaethralData ->
                            ( hypaethralJson
                            , hypaethralData
                            )
                        )
                    |> Result.withDefault
                        ( User.encodeHypaethralData User.emptyHypaethralData
                        , User.emptyHypaethralData
                        )
                    |> Commence maybeMethod
                    |> UserMsg
            )



-- 📣


update : User.Msg -> Manager
update msg =
    case msg of
        Commence a b ->
            commence a b

        SetSyncMethod a ->
            setSyncMethod a

        Sync ->
            sync { initialTask = Nothing }

        UnsetSyncMethod ->
            unsetSyncMethod

        -----------------------------------------
        -- 0. Secret Key
        -----------------------------------------
        FabricateSecretKey a ->
            fabricateSecretKey a

        SecretKeyFabricated ->
            secretKeyFabricated

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
        GotHypaethralData a ->
            gotHypaethralData a

        GotWebnativeResponse a ->
            gotWebnativeResponse a

        SaveAllHypaethralData ->
            saveAllHypaethralData

        SaveHypaethralDataBit a ->
            saveHypaethralData a

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


commence : Maybe Method -> ( Json.Value, HypaethralData ) -> Manager
commence maybeMethod ( hypaethralJson, hypaethralData ) model =
    -- 🚀
    -- Initiated from `initialCommand`.
    -- Loaded the used-sync method and the local hypaethral data.
    { model | userSyncMethod = maybeMethod }
        |> sendHypaethralDataToUI hypaethralJson hypaethralData
        -- Next load the hypaethral data from the syncing service.
        |> andThen (sync { initialTask = Nothing })


gotWebnativeResponse : Webnative.Response -> Manager
gotWebnativeResponse response model =
    let
        baggage =
            model.hypaethralRetrieval
                |> Maybe.map (Zipper.current >> Tuple3.third)
                |> Maybe.withDefault BaggageClaimed
    in
    case Fission.proceed response baggage of
        Fission.Error err ->
            Common.reportUI Alien.ReportError err model

        Fission.Hypaethral data ->
            -- TODO: hypaethralDataRetrieved data model
            Return.singleton model

        Fission.LoadedFileSystem ->
            -- Had to load the filesystem first, please continue.
            let
                userSyncMethod =
                    Fission { initialised = True }
            in
            -- TODO:
            -- model.userSyncMethod
            --     |> Maybe.withDefault userSyncMethod
            --     |> (\a -> retrieveAllHypaethralData a { model | userSyncMethod = Just a })
            Return.singleton model

        Fission.Ongoing newBaggage request ->
            model.hypaethralRetrieval
                |> Maybe.map
                    (newBaggage
                        |> always
                        |> Tuple3.mapThird
                        |> Zipper.map
                    )
                |> (\h -> { model | hypaethralRetrieval = h })
                |> Return.communicate (Ports.webnativeRequest request)

        Fission.OtherRequest request ->
            request
                |> Ports.webnativeRequest
                |> return model

        Fission.SaveNextHypaethralBit ->
            -- TODO
            -- saveNextHypaethralBit model
            Return.singleton model

        Fission.Stopping ->
            Return.singleton model


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
                |> Common.attemptTask (UserMsg << GotHypaethralData)
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

        _ ->
            Cmd.none


unsetSyncMethod : Manager
unsetSyncMethod model =
    -- 💀
    -- Unset & remove stored method.
    [ Ports.removeCache (Alien.trigger Alien.SyncMethod)
    , Ports.removeCache (Alien.trigger Alien.AuthSecretKey)

    --
    , case model.userSyncMethod of
        Just (Dropbox _) ->
            Cmd.none

        Just (Fission _) ->
            Ports.deconstructFission ()

        Just (Ipfs _) ->
            Cmd.none

        Just Local ->
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
    Alien.AuthEnclosedData
        |> Alien.trigger
        |> Ports.requestCache
        |> Return.communicate


saveEnclosedData : Json.Value -> Manager
saveEnclosedData json =
    json
        |> Alien.broadcast Alien.AuthEnclosedData
        |> Ports.toCache
        |> Return.communicate



-- 🔱  ░░  DATA - HYPAETHRAL


gotHypaethralData : HypaethralData -> Manager
gotHypaethralData hypaethralData model =
    model
        -- TODO: Don't send data to UI if not necessary
        |> sendHypaethralDataToUI (User.encodeHypaethralData hypaethralData) hypaethralData
        |> (case model.userSyncMethod of
                Just userSyncMethod ->
                    andThen (Common.giveUI Alien.AuthMethod <| encodeMethod userSyncMethod)

                Nothing ->
                    identity
           )



-- retrieveHypaethralData : Method -> HypaethralBit -> Manager
-- retrieveHypaethralData method bit model =
--     -- TODO: Remove method
--     let
--         filename =
--             hypaethralBitFileName bit
--         file =
--             Json.string filename
--     in
--     case method of
--         -- 🚀
--         Dropbox { accessToken, expiresAt, refreshToken } ->
--             let
--                 currentTime =
--                     Time.posixToMillis model.currentTime // 1000
--                 currentTimeWithOffset =
--                     -- We add 60 seconds here because we only get the current time every minute,
--                     -- so there's always the chance the "current time" is 1-60 seconds behind.
--                     currentTime + 60
--             in
--             -- If the access token is expired
--             if currentTimeWithOffset >= expiresAt then
--                 refreshToken
--                     |> Dropbox.refreshAccessToken
--                     |> Task.attempt
--                         (\result ->
--                             case result of
--                                 Ok tokens ->
--                                     bit
--                                         |> RetrieveHypaethralData method
--                                         |> RefreshedDropboxTokens
--                                             { currentTime = currentTime
--                                             , refreshToken = refreshToken
--                                             }
--                                             tokens
--                                         |> UserMsg
--                                 Err err ->
--                                     Common.reportUICmdMsg Alien.ReportError err
--                         )
--                     |> return model
--             else
--                 [ ( "file", file )
--                 , ( "token", Json.string accessToken )
--                 ]
--                     |> Json.object
--                     |> Alien.broadcast Alien.AuthDropbox
--                     |> Ports.requestDropbox
--                     |> return model
--         Fission params ->
--             filename
--                 |> Fission.retrieve params bit
--                 |> Ports.webnativeRequest
--                 |> return model
--         Ipfs { apiOrigin } ->
--             [ ( "apiOrigin", Json.string apiOrigin )
--             , ( "file", file )
--             ]
--                 |> Json.object
--                 |> Alien.broadcast Alien.AuthIpfs
--                 |> Ports.requestIpfs
--                 |> return model
--         Local ->
--             [ ( "file", file ) ]
--                 |> Json.object
--                 |> Alien.broadcast Alien.AuthAnonymous
--                 |> Ports.requestCache
--                 |> return model
--         RemoteStorage { userAddress, token } ->
--             [ ( "file", file )
--             , ( "token", Json.string token )
--             , ( "userAddress", Json.string userAddress )
--             ]
--                 |> Json.object
--                 |> Alien.broadcast Alien.AuthRemoteStorage
--                 |> Ports.requestRemoteStorage
--                 |> return model


saveAllHypaethralData : Manager
saveAllHypaethralData =
    User.hypaethralBit.list
        |> List.map Tuple.second
        |> saveHypaethralDataBits


saveHypaethralData : HypaethralBit -> Manager
saveHypaethralData bit model =
    let
        method =
            -- TODO
            Maybe.withDefault Local model.userSyncMethod

        filename =
            hypaethralBitFileName bit

        file =
            Json.string filename

        userData =
            model.hypaethralUserData

        updatedUserData =
            { userData | modifiedAt = Just model.currentTime }

        updatedModel =
            { model | hypaethralUserData = updatedUserData }

        json =
            encodeHypaethralBit bit updatedUserData
    in
    case method of
        -- 🚀
        Dropbox { accessToken, expiresAt, refreshToken } ->
            let
                currentTime =
                    Time.posixToMillis model.currentTime // 1000

                currentTimeWithOffset =
                    -- We add 60 seconds here because we only get the current time every minute,
                    -- so there's always the chance the "current time" is 1-60 seconds behind.
                    currentTime + 60
            in
            -- If the access token is expired
            if currentTimeWithOffset >= expiresAt then
                refreshToken
                    |> Dropbox.refreshAccessToken
                    |> Task.attempt
                        (\result ->
                            case result of
                                Ok tokens ->
                                    bit
                                        |> SaveHypaethralDataBit
                                        |> RefreshedDropboxTokens
                                            { currentTime = currentTime
                                            , refreshToken = refreshToken
                                            }
                                            tokens
                                        |> UserMsg

                                Err err ->
                                    Common.reportUICmdMsg Alien.ReportError err
                        )
                    |> return updatedModel

            else
                [ ( "data", json )
                , ( "file", file )
                , ( "token", Json.string accessToken )
                ]
                    |> Json.object
                    |> Alien.broadcast Alien.AuthDropbox
                    |> Ports.toDropbox
                    |> return updatedModel

        Fission params ->
            json
                |> Fission.save params bit filename
                |> List.map Ports.webnativeRequest
                |> Cmd.batch
                |> return updatedModel

        Ipfs { apiOrigin } ->
            [ ( "apiOrigin", Json.string apiOrigin )
            , ( "data", json )
            , ( "file", file )
            ]
                |> Json.object
                |> Alien.broadcast Alien.AuthIpfs
                |> Ports.toIpfs
                |> return updatedModel

        Local ->
            [ ( "data", json )
            , ( "file", file )
            ]
                |> Json.object
                |> Alien.broadcast Alien.AuthAnonymous
                |> Ports.toCache
                |> return updatedModel

        RemoteStorage { userAddress, token } ->
            [ ( "data", json )
            , ( "file", file )
            , ( "token", Json.string token )
            , ( "userAddress", Json.string userAddress )
            ]
                |> Json.object
                |> Alien.broadcast Alien.AuthRemoteStorage
                |> Ports.toRemoteStorage
                |> return updatedModel



-- saveHypaethralDataLocallyToo =
--     [ ( "data", json )
--     , ( "file", file )
--     ]
--         |> Json.object
--         |> Alien.broadcast Alien.SyncLocal
--         |> Ports.toCache
--         |> return model


{-| Save different parts of hypaethral data,
one part at a time.
-}
saveHypaethralDataBits : List HypaethralBit -> Manager
saveHypaethralDataBits bits model =
    -- TODO:
    Return.singleton model


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
        |> andThen (saveHypaethralDataBits bits)


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
        |> Alien.broadcast Alien.SyncMethod
        |> Ports.toCache
        |> return { model | userSyncMethod = Just method }



-- 🔱  ░░  SECRET KEY


fabricateSecretKey : String -> Manager
fabricateSecretKey passphrase =
    passphrase
        |> Json.string
        |> Alien.broadcast Alien.FabricateSecretKey
        |> Ports.fabricateSecretKey
        |> Return.communicate


removeEncryptionKey : Manager
removeEncryptionKey =
    [ Alien.AuthSecretKey
        |> Alien.trigger
        |> Ports.removeCache

    --
    , SaveAllHypaethralData
        |> UserMsg
        |> do
    ]
        |> Cmd.batch
        |> Return.communicate


secretKeyFabricated : Manager
secretKeyFabricated model =
    -- if model.performingSignIn then
    --     retrieveAllHypaethralData model
    --
    -- else
    --     saveAllHypaethralData model
    -- TODO:
    sync { initialTask = Nothing } model


updateEncryptionKey : Json.Value -> Manager
updateEncryptionKey json =
    case Decode.decodeValue Decode.string json of
        Ok passphrase ->
            -- TODO: update with task
            -- 1. fabricateSecretKey passphrase
            -- 2. saveAllHypaethralData model
            Return.singleton

        Err _ ->
            Return.singleton



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
