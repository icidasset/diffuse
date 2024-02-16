module UI.Other.State exposing (..)

import Alien
import Common exposing (ServiceWorkerStatus(..))
import Notifications
import Return exposing (return)
import Time
import UI.Common.State as Common
import UI.Ports as Ports
import UI.Types exposing (..)



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
        syncHypaethralData { model | isOnline = bool }

    else
        Return.singleton { model | isOnline = bool }


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
