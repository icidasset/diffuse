module UI.Authentication.State exposing (..)

import Browser.Navigation as Nav
import Common exposing (Switch(..))
import Http
import Json.Decode as Json
import Management
import Monocle.Lens as Lens
import Notifications
import Return exposing (andThen, return)
import Return.Ext as Return exposing (communicate)
import UI.Common.State as Common exposing (showNotification, showNotificationWithModel)
import UI.Reply as Reply exposing (Reply(..))
import UI.Reply.Translate as Reply
import UI.Types as UI exposing (Manager, Msg(..))
import User.Layer.Methods.RemoteStorage as RemoteStorage



-- 🔱


authenticationBootFailure : String -> Manager
authenticationBootFailure err model =
    model
        |> showNotification (Notifications.error err)
        |> andThen (Reply.translate LoadDefaultBackdrop)


missingSecretKey : Json.Value -> Manager
missingSecretKey _ model =
    "There seems to be existing data that's encrypted, I will need the passphrase (ie. encryption key) to continue."
        |> Notifications.error
        |> showNotificationWithModel model
        |> andThen (Reply.translate <| Reply.LoadDefaultBackdrop)
        |> andThen (Reply.translate <| Reply.ToggleLoadingScreen Off)


notAuthenticated : Manager
notAuthenticated model =
    -- This is the message we get when the app initially
    -- finds out we're not authenticated.
    andThen
        (Reply.translate Reply.LoadDefaultBackdrop)
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
