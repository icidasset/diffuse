module UI.Authentication.State exposing (..)

import Alien
import Base64
import Binary
import Browser.Navigation as Nav
import Chunky exposing (..)
import Common exposing (Switch(..))
import Conditional exposing (..)
import Html exposing (a)
import Html.Attributes exposing (placeholder, value)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Decode as Json
import Json.Encode
import Lens.Ext as Lens
import Management
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Monocle.Lens as Lens exposing (Lens)
import Notifications
import Return exposing (andThen, return)
import Return.Ext as Return
import SHA
import String.Ext as String
import UI.Authentication.Types as Authentication exposing (..)
import UI.Backdrop as Backdrop
import UI.Common.State as Common exposing (showNotification, showNotificationWithModel)
import UI.Interface.State as Interface
import UI.Ports as Ports
import UI.Reply as Reply exposing (Reply(..))
import UI.Reply.Translate as Reply
import UI.Types as UI exposing (..)
import Url exposing (Url)
import Url.Ext as Url
import User.Layer exposing (..)
import User.Layer.Methods.RemoteStorage as RemoteStorage



-- ⛩


minimumPassphraseLength =
    16


passphraseLengthErrorMessage =
    "Your passphrase should be atleast *16 characters* long."



-- 🌳


initialModel : Url -> Authentication.State
initialModel url =
    case Url.action url of
        [ "authenticate", "dropbox" ] ->
            url.fragment
                |> Maybe.map (String.split "&")
                |> Maybe.map (List.filter <| String.startsWith "access_token=")
                |> Maybe.andThen List.head
                |> Maybe.withDefault ""
                |> String.replace "access_token=" ""
                |> (\t ->
                        NewEncryptionKeyScreen
                            (Dropbox { token = t })
                            Nothing
                   )

        [ "authenticate", "remotestorage", encodedUserAddress ] ->
            let
                userAddress =
                    encodedUserAddress
                        |> Url.percentDecode
                        |> Maybe.andThen (Base64.decode >> Result.toMaybe)
                        |> Maybe.withDefault encodedUserAddress
            in
            url.fragment
                |> Maybe.map (String.split "&")
                |> Maybe.map (List.filter <| String.startsWith "access_token=")
                |> Maybe.andThen List.head
                |> Maybe.withDefault ""
                |> String.replace "access_token=" ""
                |> (\t ->
                        NewEncryptionKeyScreen
                            (RemoteStorage { userAddress = userAddress, token = t })
                            Nothing
                   )

        _ ->
            Welcome


lens : Lens UI.Model Authentication.State
lens =
    { get = .authentication
    , set = \a m -> { m | authentication = a }
    }



-- 📣


update : Authentication.Msg -> Manager
update msg =
    case msg of
        Authentication.Bypass ->
            Return.singleton

        CancelFlow ->
            cancelFlow

        GetStarted ->
            startFlow

        ShowMoreOptions a ->
            showMoreOptions a

        SignIn a ->
            signIn a

        SignInWithPassphrase a b ->
            signInWithPassphrase a b

        SignedIn a ->
            signedIn a

        TriggerExternalAuth a b ->
            externalAuth a b

        -----------------------------------------
        -- Encryption
        -----------------------------------------
        KeepPassphraseInMemory a ->
            keepPassphraseInMemory a

        RemoveEncryptionKey a ->
            removeEncryptionKey a

        ShowNewEncryptionKeyScreen a ->
            showNewEncryptionKeyScreen a

        Authentication.ShowUpdateEncryptionKeyScreen a ->
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

        Input a ->
            input a

        ConfirmInput ->
            confirmInput

        -----------------------------------------
        -- Textile
        -----------------------------------------
        PingTextile ->
            pingTextile

        PingTextileCallback a ->
            pingTextileCallback a

        PingOtherTextile a ->
            pingOtherTextile a

        PingOtherTextileCallback a b ->
            pingOtherTextileCallback a b


organize : Organizer Authentication.State -> Manager
organize =
    Management.organize lens


replaceState : Authentication.State -> Manager
replaceState state =
    lens.set state >> Return.singleton



-- 🔱


bootFailure : String -> Manager
bootFailure err model =
    model
        |> showNotification (Notifications.error err)
        |> andThen Backdrop.setDefault


cancelFlow : Manager
cancelFlow model =
    (\state ->
        case state of
            Authenticated method ->
                Authenticated method

            InputScreen _ _ ->
                Unauthenticated

            NewEncryptionKeyScreen _ _ ->
                Unauthenticated

            UpdateEncryptionKeyScreen method _ ->
                Authenticated method

            Unauthenticated ->
                Welcome

            Welcome ->
                Welcome
    )
        |> Lens.adjust lens model
        |> Return.singleton
        |> Return.andThen (Reply.translate Reply.ForceTracksRerender)


externalAuth : Method -> String -> Manager
externalAuth method string model =
    case method of
        Blockstack ->
            Alien.RedirectToBlockstackSignIn
                |> Alien.trigger
                |> Ports.toBrain
                |> return model

        Dropbox _ ->
            [ ( "response_type", "token" )
            , ( "client_id", "te0c9pbeii8f8bw" )
            , ( "redirect_uri", Common.urlOrigin model.url ++ "?action=authenticate/dropbox" )
            ]
                |> Common.queryString
                |> String.append "https://www.dropbox.com/oauth2/authorize"
                |> Nav.load
                |> return model

        RemoteStorage _ ->
            string
                |> RemoteStorage.parseUserAddress
                |> Maybe.map
                    (RemoteStorage.webfingerRequest RemoteStorageWebfinger)
                |> Maybe.unwrap
                    (RemoteStorage.userAddressError
                        |> Notifications.error
                        |> Common.showNotificationWithModel model
                    )
                    (return model)

        _ ->
            Return.singleton model


missingSecretKey : Json.Value -> Manager
missingSecretKey _ model =
    "There seems to be existing data that's encrypted, I will need the passphrase (ie. encryption key) to continue."
        |> Notifications.error
        |> showNotificationWithModel model
        |> andThen Backdrop.setDefault
        |> andThen (Interface.toggleLoadingScreen Off)


notAuthenticated : Manager
notAuthenticated model =
    -- This is the message we get when the app initially
    -- finds out we're not authenticated.
    andThen
        Backdrop.setDefault
        (if model.isUpgrading then
            """
            Thank you for using Diffuse V1!
            If you want to import your old data,
            please pick the storage method you used before and
            go to the [import page](#/settings/import-export).
            """
                |> Notifications.stickySuccess
                |> showNotificationWithModel { model | isUpgrading = False }

         else
            Return.singleton model
        )


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


showMoreOptions : Mouse.Event -> Manager
showMoreOptions mouseEvent =
    ( mouseEvent.clientPos
    , mouseEvent.offsetPos
    )
        |> (\( ( a, b ), ( c, d ) ) ->
                { x = a - c + 15
                , y = b - d + 12
                }
           )
        |> ShowMoreAuthenticationOptions
        |> Reply
        |> Return.performance


signedIn : Method -> Manager
signedIn method =
    replaceState (Authenticated method)


signIn : Method -> Manager
signIn method model =
    [ ( "method", encodeMethod method )
    , ( "passphrase", Json.Encode.null )
    ]
        |> Json.Encode.object
        |> Alien.broadcast Alien.SignIn
        |> Ports.toBrain
        --
        |> Return.return model
        |> Return.andThen (Interface.toggleLoadingScreen On)


signInWithPassphrase : Method -> String -> Manager
signInWithPassphrase method passphrase model =
    if String.length passphrase < minimumPassphraseLength then
        passphraseLengthErrorMessage
            |> Notifications.error
            |> Common.showNotificationWithModel model

    else
        [ ( "method", encodeMethod method )
        , ( "passphrase", Json.Encode.string <| hashPassphrase passphrase )
        ]
            |> Json.Encode.object
            |> Alien.broadcast Alien.SignIn
            |> Ports.toBrain
            --
            |> Return.return model
            |> Return.andThen (Interface.toggleLoadingScreen On)


startFlow : Manager
startFlow =
    replaceState Unauthenticated



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


removeEncryptionKey : Method -> Manager
removeEncryptionKey method model =
    Alien.RemoveEncryptionKey
        |> Alien.trigger
        |> Ports.toBrain
        --
        |> Return.return (lens.set (Authenticated method) model)
        |> Return.andThen (Common.showNotification <| Notifications.success "Saving data without encryption ...")
        |> Return.andThen (Reply.translate ForceTracksRerender)


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
            |> Return.return (lens.set (Authenticated method) model)
            |> Return.andThen (Common.showNotification <| Notifications.success "Encrypting data with new passphrase ...")
            |> Return.andThen (Reply.translate ForceTracksRerender)



-- IPFS


pingIpfs : Manager
pingIpfs model =
    { url = "//localhost:5001/api/v0/id"
    , expect = Http.expectWhatever (AuthenticationMsg << PingIpfsCallback)
    }
        |> Http.get
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
                { placeholder = "//localhost:5001"
                , question = """
                    Where's your IPFS API located?<br />
                    <span class="font-normal text-white-60">
                        You can find this address on the IPFS Web UI.<br />
                        Most likely you'll also need to setup CORS.<br />
                        You can find the instructions for that
                        <a href="about#CORS__IPFS" target="_blank" class="border-b border-current-color font-semibold inline-block leading-tight">here</a>.
                    </span>
                  """
                , value = "//localhost:5001"
                }


pingOtherIpfs : String -> Manager
pingOtherIpfs origin model =
    { url = origin ++ "/api/v0/id"
    , expect = Http.expectWhatever (AuthenticationMsg << PingOtherIpfsCallback origin)
    }
        |> Http.get
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
askForInput method q =
    { placeholder = q.placeholder
    , question = q.question
    , value = q.value
    }
        |> InputScreen method
        |> replaceState


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


confirmInput : Manager
confirmInput model =
    case lens.get model of
        InputScreen (Ipfs i) { value } ->
            pingOtherIpfs (String.chopEnd "/" value) model

        InputScreen (RemoteStorage r) { value } ->
            externalAuth (RemoteStorage r) value model

        InputScreen (Textile t) { value } ->
            pingOtherTextile (String.chopEnd "/" value) model

        _ ->
            Return.singleton model



-- TEXTILE


pingTextile : Manager
pingTextile model =
    { url = "//localhost:40600/api/v0/summary"
    , expect = Http.expectWhatever (AuthenticationMsg << PingTextileCallback)
    }
        |> Http.get
        |> return model


pingTextileCallback : Result Http.Error () -> Manager
pingTextileCallback result =
    case result of
        Ok _ ->
            { apiOrigin = "//localhost:40600" }
                |> Textile
                |> signIn

        Err _ ->
            askForInput
                (Textile { apiOrigin = "" })
                { placeholder = "//localhost:40600"
                , question = """
                Where's your Textile API located?<br />
                <span class="font-normal text-white-60">
                    You might need to do some CORS configuration.<br />
                    You can find the instructions for that
                    <a href="about#CORS__Textile" target="_blank" class="border-b border-current-color font-semibold inline-block leading-tight">here</a>.<br />
                    You can't connect to a HTTP server while on HTTPS.
                </span>
              """
                , value = "//localhost:40600"
                }


pingOtherTextile : String -> Manager
pingOtherTextile origin model =
    { url = origin ++ "/api/v0/summary"
    , expect = Http.expectWhatever (AuthenticationMsg << PingOtherTextileCallback origin)
    }
        |> Http.get
        |> return model


pingOtherTextileCallback : String -> Result Http.Error () -> Manager
pingOtherTextileCallback origin result =
    case result of
        Ok _ ->
            { apiOrigin = origin }
                |> Textile
                |> signIn

        Err _ ->
            "Can't reach this Textile API, maybe it's offline? Or I don't have access?"
                |> Notifications.error
                |> Common.showNotification



-- 🛠


hashPassphrase : String -> String
hashPassphrase phrase =
    phrase
        |> Binary.fromStringAsUtf8
        |> SHA.sha256
        |> Binary.toHex
        |> String.toLower
