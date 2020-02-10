module UI.Common.State exposing (..)

import Monocle.Lens as Lens exposing (Lens)
import Notifications exposing (Notification)
import Return
import UI.Notifications
import UI.Reply exposing (Reply)
import UI.Types as UI



-- 📣


showNotification : Notification Reply -> UI.Manager
showNotification notification model =
    model.notifications
        |> UI.Notifications.show notification
        |> Return.map (\n -> { model | isLoading = False, notifications = n })
        |> Return.mapCmd UI.Reply


showNotificationWithModel : UI.Model -> Notification Reply -> ( UI.Model, Cmd UI.Msg )
showNotificationWithModel model notification =
    showNotification notification model



-- 🛠


modifySingleton : Lens a b -> (b -> b) -> a -> ( a, Cmd msg )
modifySingleton lens modifier =
    Lens.modify lens modifier >> Return.singleton
