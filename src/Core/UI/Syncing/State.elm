module UI.Syncing.State exposing (..)

import Alien
import Base64
import Binary
import Browser.Navigation as Nav
import Common
import Coordinates
import Dict
import Html
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Http
import Http.Ext as Http
import Json.Decode as Json
import Json.Encode
import Lens.Ext as Lens
import Management
import Maybe.Extra as Maybe
import Monocle.Lens exposing (Lens)
import Notifications
import Return exposing (andThen, return)
import SHA
import String.Ext as String
import Svg
import Time
import UI.Backdrop as Backdrop
import UI.Common.State as Common exposing (showNotification, showNotificationWithModel)
import UI.Ports as Ports
import UI.Sources.State as Sources
import UI.Svg.Elements
import UI.Syncing.ContextMenu as Syncing
import UI.Syncing.Types as Syncing exposing (..)
import UI.Types as UI exposing (..)
import Url exposing (Protocol(..), Url)
import Url.Ext as Url
import UrlBase64
import User.Layer exposing (..)
import User.Layer.Methods.Dropbox as Dropbox
import User.Layer.Methods.RemoteStorage as RemoteStorage



-- ⛩


minimumPassphraseLength : Int
minimumPassphraseLength =
    16


passphraseLengthErrorMessage : String
passphraseLengthErrorMessage =
    "Your passphrase should be atleast *16 characters* long."



-- 🌳


initialModel : Url -> Syncing.State
initialModel url =
    case Url.action url of
        [ "authenticate", "remotestorage", encodedUserAddress ] ->
            let
                dict =
                    Url.queryDictionary { url | query = url.fragment }

                userAddress =
                    encodedUserAddress
                        |> Url.percentDecode
                        |> Maybe.andThen (UrlBase64.decode Base64.decode >> Result.toMaybe)
                        |> Maybe.withDefault encodedUserAddress
            in
            case Dict.get "access_token" dict of
                Just t ->
                    NewEncryptionKeyScreen
                        (RemoteStorage
                            { userAddress = userAddress
                            , token = t
                            }
                        )
                        Nothing

                Nothing ->
                    NotSynced

        _ ->
            NotSynced


initialCommand : Url -> Cmd Syncing.Msg
initialCommand url =
    case Url.action url of
        [ "authenticate", "dropbox" ] ->
            case Dict.get "code" (Url.queryDictionary url) of
                Just code ->
                    Dropbox.exchangeAuthCode
                        ExchangeDropboxAuthCode
                        url
                        code

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


lens : Lens UI.Model Syncing.State
lens =
    { get = .syncing
    , set = \a m -> { m | syncing = a }
    }



-- 📣


update : Syncing.Msg -> Manager
update msg =
    case msg of
        Syncing.Bypass ->
            Return.singleton

        ActivateSync a ->
            activateSync a

        ActivateSyncWithPassphrase a b ->
            activateSyncWithPassphrase a b

        BootFailure a ->
            bootFailure a

        ExchangeDropboxAuthCode a ->
            exchangeDropboxAuthCode a

        GotSyncMethod a ->
            gotSyncMethod a

        RemoteStorageWebfinger a b ->
            remoteStorageWebfinger a b

        ShowSyncDataMenu a ->
            showSyncDataMenu a

        StartedSyncing a ->
            startedSyncing a

        StopSync ->
            stopSync

        TriggerExternalAuth a b ->
            externalAuth a b

        -----------------------------------------
        -- Encryption
        -----------------------------------------
        KeepPassphraseInMemory a ->
            keepPassphraseInMemory a

        NeedEncryptionKey a ->
            needEncryptionKey a

        RemoveEncryptionKey a ->
            removeEncryptionKey a

        ShowNewEncryptionKeyScreen a ->
            showNewEncryptionKeyScreen a

        ShowUpdateEncryptionKeyScreen a ->
            showUpdateEncryptionKeyScreen a

        UpdateEncryptionKey a b ->
            updateEncryptionKey a b

        -----------------------------------------
        -- IPFS
        -----------------------------------------
        PingIpfs ->
            pingIpfs

        PingIpfsCallback a ->
            pingIpfsCallback a

        PingOtherIpfs a ->
            pingOtherIpfs a

        PingOtherIpfsCallback a b ->
            pingOtherIpfsCallback a b

        -----------------------------------------
        -- More Input
        -----------------------------------------
        AskForInput a b ->
            askForInput a b

        CancelInput ->
            cancelInput

        ConfirmInput ->
            confirmInput

        Input a ->
            input a


organize : Organizer Syncing.State -> Manager
organize =
    Management.organize lens


replaceState : Syncing.State -> Manager
replaceState state =
    lens.set state >> Return.singleton



-- 🔱


activateSync : Method -> Manager
activateSync method model =
    [ ( "method", encodeMethod method )
    , ( "passphrase", Json.Encode.null )
    ]
        |> Json.Encode.object
        |> Alien.broadcast Alien.SetSyncMethod
        |> Ports.toBrain
        --
        |> return model


activateSyncWithPassphrase : Method -> String -> Manager
activateSyncWithPassphrase method passphrase model =
    if String.length passphrase < minimumPassphraseLength then
        passphraseLengthErrorMessage
            |> Notifications.error
            |> Common.showNotificationWithModel model

    else
        [ ( "method", encodeMethod method )
        , ( "passphrase", Json.Encode.string <| hashPassphrase passphrase )
        ]
            |> Json.Encode.object
            |> Alien.broadcast Alien.SetSyncMethod
            |> Ports.toBrain
            --
            |> return model


bootFailure : String -> Manager
bootFailure err model =
    model
        |> showNotification (Notifications.error err)
        |> andThen Backdrop.setDefault


externalAuth : Method -> String -> Manager
externalAuth method string model =
    case method of
        Dropbox _ ->
            [ ( "client_id", Dropbox.clientId )
            , ( "redirect_uri", Dropbox.redirectUri model.url )
            , ( "response_type", "code" )
            , ( "token_access_type", "offline" )
            ]
                |> Common.queryString
                |> String.append "https://www.dropbox.com/oauth2/authorize"
                |> Nav.load
                |> return model

        RemoteStorage _ ->
            string
                |> RemoteStorage.parseUserAddress
                |> Maybe.map (RemoteStorage.webfingerRequest RemoteStorageWebfinger model.url.protocol)
                |> Maybe.map (Cmd.map SyncingMsg)
                |> Maybe.unwrap
                    (RemoteStorage.userAddressError
                        |> Notifications.error
                        |> Common.showNotificationWithModel model
                    )
                    (return model)

        _ ->
            Return.singleton model


exchangeDropboxAuthCode : Result Http.Error Dropbox.Tokens -> Manager
exchangeDropboxAuthCode result model =
    case result of
        Ok tokens ->
            case tokens.refreshToken of
                Just refreshToken ->
                    Nothing
                        |> NewEncryptionKeyScreen
                            (Dropbox
                                { accessToken = tokens.accessToken
                                , expiresAt = Time.posixToMillis model.currentTime // 1000 + tokens.expiresIn
                                , refreshToken = refreshToken
                                }
                            )
                        |> Lens.replace lens model
                        |> Return.singleton

                Nothing ->
                    "Missing refresh token in Dropbox code exchange flow."
                        |> Notifications.stickyError
                        |> showNotificationWithModel
                            (Lens.replace lens model NotSynced)

        Err err ->
            []
                |> Notifications.errorWithCode
                    "Failed to authenticate with Dropbox"
                    (Http.errorToString err)
                |> showNotificationWithModel
                    (Lens.replace lens model NotSynced)


gotSyncMethod : Json.Value -> Manager
gotSyncMethod json model =
    let
        afterwards a =
            andThen
                (\m ->
                    if m.processAutomatically then
                        Sources.process m

                    else
                        Return.singleton m
                )
                (case model.syncing of
                    Syncing { notificationId } ->
                        Common.dismissNotification { id = notificationId } a

                    _ ->
                        Return.singleton a
                )
    in
    -- 🧠 told me which auth method we're using,
    -- so we can tell the user in the UI.
    case decodeMethod json of
        Just method ->
            model
                |> replaceState (Synced method)
                |> andThen afterwards

        Nothing ->
            afterwards model


remoteStorageWebfinger : RemoteStorage.Attributes -> Result Http.Error String -> Manager
remoteStorageWebfinger remoteStorage result model =
    case result of
        Ok oauthOrigin ->
            let
                origin =
                    Common.urlOrigin model.url
            in
            remoteStorage
                |> RemoteStorage.oauthAddress
                    { oauthOrigin = oauthOrigin
                    , origin = origin
                    }
                |> Nav.load
                |> return model

        Err _ ->
            RemoteStorage.webfingerError
                |> Notifications.error
                |> showNotificationWithModel model


showSyncDataMenu : Mouse.Event -> Manager
showSyncDataMenu mouseEvent model =
    mouseEvent.clientPos
        |> Coordinates.fromTuple
        |> Syncing.syncDataMenu
        |> Common.showContextMenuWithModel model


startedSyncing : Json.Value -> Manager
startedSyncing json =
    -- 🧠 started syncing
    case decodeMethod json of
        Just method ->
            Common.showSyncingNotification method

        Nothing ->
            Return.singleton


stopSync : Manager
stopSync model =
    Alien.UnsetSyncMethod
        |> Alien.trigger
        |> Ports.toBrain
        |> return model
        |> andThen (replaceState NotSynced)



-- ENCRYPTION


keepPassphraseInMemory : String -> Manager
keepPassphraseInMemory passphrase model =
    (\state ->
        case state of
            NewEncryptionKeyScreen method _ ->
                NewEncryptionKeyScreen method (Just passphrase)

            UpdateEncryptionKeyScreen method _ ->
                UpdateEncryptionKeyScreen method (Just passphrase)

            s ->
                s
    )
        |> Lens.adjust lens model
        |> Return.singleton


needEncryptionKey : { error : String } -> Manager
needEncryptionKey { error } model =
    (case lens.get model of
        Syncing { notificationId } ->
            Common.dismissNotification { id = notificationId } model

        m ->
            replaceState m model
    )
        |> andThen
            (error
                |> Notifications.stickyError
                |> Common.showNotification
            )
        |> andThen
            stopSync


removeEncryptionKey : Method -> Manager
removeEncryptionKey method model =
    Alien.RemoveEncryptionKey
        |> Alien.trigger
        |> Ports.toBrain
        --
        |> return
            (lens.set (Synced method) model)
        |> andThen
            ("Saving data without encryption ..."
                |> Notifications.success
                |> Common.showNotification
            )
        |> andThen
            Common.forceTracksRerender


showNewEncryptionKeyScreen : Method -> Manager
showNewEncryptionKeyScreen method =
    replaceState (NewEncryptionKeyScreen method Nothing)


showUpdateEncryptionKeyScreen : Method -> Manager
showUpdateEncryptionKeyScreen method =
    replaceState (UpdateEncryptionKeyScreen method Nothing)


updateEncryptionKey : Method -> String -> Manager
updateEncryptionKey method passphrase model =
    if String.length passphrase < minimumPassphraseLength then
        passphraseLengthErrorMessage
            |> Notifications.error
            |> Common.showNotificationWithModel model

    else
        passphrase
            |> hashPassphrase
            |> Json.Encode.string
            |> Alien.broadcast Alien.UpdateEncryptionKey
            |> Ports.toBrain
            --
            |> return
                (lens.set (Synced method) model)
            |> andThen
                ("Encrypting data with new passphrase ..."
                    |> Notifications.success
                    |> Common.showNotification
                )
            |> andThen
                Common.forceTracksRerender



-- IPFS


pingIpfs : Manager
pingIpfs model =
    case model.url.protocol of
        Https ->
            """
            Unfortunately the local IPFS API doesn't work with HTTPS.
            Install the [IPFS Companion](https://github.com/ipfs-shipyard/ipfs-companion#release-channel) browser extension to get around this issue
            (and make sure it redirects to the local gateway).
            """
                |> Notifications.error
                |> Common.showNotificationWithModel model

        Http ->
            { url = "//localhost:5001/api/v0/id"
            , expect = Http.expectWhatever (SyncingMsg << PingIpfsCallback)
            , body = Http.emptyBody
            }
                |> Http.post
                |> return model


pingIpfsCallback : Result Http.Error () -> Manager
pingIpfsCallback result =
    case result of
        Ok _ ->
            { apiOrigin = "//localhost:5001" }
                |> Ipfs
                |> showNewEncryptionKeyScreen

        Err _ ->
            askForInput
                (Ipfs { apiOrigin = "" })
                { icon = \size _ -> Svg.map never (UI.Svg.Elements.ipfsLogo size)
                , placeholder = "//localhost:5001"
                , question =
                    { question =
                        "Where's your IPFS API located?"
                    , info =
                        [ Html.text "You can find this address on the IPFS Web UI."
                        , Html.br [] []
                        , Html.text "Most likely you'll also need to setup CORS."
                        , Html.br [] []
                        , Html.text "You can find the instructions for that "
                        , Html.a
                            [ Html.Attributes.class "border-b border-current-color font-semibold inline-block leading-tight"
                            , Html.Attributes.href "about/cors/#CORS__IPFS"
                            , Html.Attributes.target "_blank"
                            ]
                            [ Html.text "here" ]
                        ]
                    }
                , value = "//localhost:5001"
                }


pingOtherIpfs : String -> Manager
pingOtherIpfs origin model =
    { url = origin ++ "/api/v0/id"
    , expect = Http.expectWhatever (SyncingMsg << PingOtherIpfsCallback origin)
    , body = Http.emptyBody
    }
        |> Http.post
        |> return model


pingOtherIpfsCallback : String -> Result Http.Error () -> Manager
pingOtherIpfsCallback origin result =
    case result of
        Ok _ ->
            { apiOrigin = origin }
                |> Ipfs
                |> showNewEncryptionKeyScreen

        Err _ ->
            "Can't reach this IPFS API, maybe it's offline? Or I don't have access?"
                |> Notifications.error
                |> Common.showNotification



-- MORE INPUT


askForInput : Method -> Question -> Manager
askForInput method question =
    question
        |> InputScreen method
        |> replaceState


cancelInput : Manager
cancelInput model =
    case lens.get model of
        InputScreen _ _ ->
            replaceState NotSynced model

        NewEncryptionKeyScreen _ _ ->
            replaceState NotSynced model

        UpdateEncryptionKeyScreen method _ ->
            replaceState (Synced method) model

        m ->
            replaceState m model


confirmInput : Manager
confirmInput model =
    case lens.get model of
        InputScreen (Ipfs _) { value } ->
            pingOtherIpfs (String.chopEnd "/" value) model

        InputScreen (RemoteStorage r) { value } ->
            externalAuth (RemoteStorage r) value model

        _ ->
            Return.singleton model


input : String -> Manager
input string model =
    (\state ->
        case state of
            InputScreen method opts ->
                InputScreen method { opts | value = string }

            s ->
                s
    )
        |> Lens.adjust lens model
        |> Return.singleton



-- 🛠


hashPassphrase : String -> String
hashPassphrase phrase =
    phrase
        |> Binary.fromStringAsUtf8
        |> SHA.sha256
        |> Binary.toHex
        |> String.toLower
