module UI.Other.State exposing (..)

import Alien
import Common exposing (ServiceWorkerStatus(..))
import Notifications
import Return exposing (return)
import Time
import UI.Authentication.Types as Authentication
import UI.Common.State as Common
import UI.Ports as Ports
import UI.Types exposing (..)
import User.Layer exposing (..)



-- 🔱


installedServiceWorker : Manager
installedServiceWorker model =
    case model.serviceWorkerStatus of
        InstallingNew ->
            Return.singleton { model | serviceWorkerStatus = WaitingForActivation }

        _ ->
            Return.singleton { model | serviceWorkerStatus = Activated }


installingServiceWorker : Manager
installingServiceWorker model =
    Return.singleton { model | serviceWorkerStatus = InstallingNew }


redirectToBrain : Alien.Event -> Manager
redirectToBrain event model =
    return model (Ports.toBrain event)


reloadApp : Manager
reloadApp model =
    return model (Ports.reloadApp ())


setIsOnline : Bool -> Manager
setIsOnline bool model =
    if bool then
        -- We're caching the user's data in the browser while offline.
        -- If we're back online again, sync all the user's data.
        (case model.authentication of
            Authentication.Authenticated (Dropbox _) ->
                syncHypaethralData

            Authentication.Authenticated (RemoteStorage _) ->
                syncHypaethralData

            _ ->
                Return.singleton
        )
            { model | isOnline = True }

    else
        -- The app went offline, cache everything
        -- (if caching is supported).
        ( { model | isOnline = False }
        , case model.authentication of
            Authentication.Authenticated (Dropbox _) ->
                Ports.toBrain (Alien.trigger Alien.SyncHypaethralData)

            Authentication.Authenticated (RemoteStorage _) ->
                Ports.toBrain (Alien.trigger Alien.SyncHypaethralData)

            _ ->
                Cmd.none
        )


setCurrentTime : Time.Posix -> Manager
setCurrentTime time model =
    Return.singleton { model | currentTime = time }


setCurrentTimeZone : Time.Zone -> Manager
setCurrentTimeZone zone model =
    Return.singleton { model | currentTimeZone = zone }



-- ⚗️


syncHypaethralData : Manager
syncHypaethralData model =
    model
        |> Common.showNotification (Notifications.casual "Syncing")
        |> Return.command (Ports.toBrain <| Alien.trigger Alien.SyncHypaethralData)
