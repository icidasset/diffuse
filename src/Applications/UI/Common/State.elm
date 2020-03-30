module UI.Common.State exposing (..)

import Browser.Navigation as Nav
import ContextMenu exposing (ContextMenu)
import Monocle.Lens as Lens exposing (Lens)
import Notifications exposing (Notification)
import Return exposing (return)
import UI.Notifications
import UI.Page as Page exposing (Page)
import UI.Reply exposing (Reply)
import UI.Types as UI exposing (Manager)



-- 📣


changeUrlUsingPage : Page -> Manager
changeUrlUsingPage page model =
    page
        |> Page.toString
        |> Nav.pushUrl model.navKey
        |> return model


dismissNotification : { id : Int } -> Manager
dismissNotification options model =
    options
        |> UI.Notifications.dismiss model.notifications
        |> Return.map (\n -> { model | notifications = n })
        |> Return.mapCmd UI.Reply


showContextMenuWithModel : UI.Model -> ContextMenu Reply -> ( UI.Model, Cmd UI.Msg )
showContextMenuWithModel model contextMenu =
    Return.singleton { model | contextMenu = Just contextMenu }


showNotification : Notification Reply -> Manager
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
