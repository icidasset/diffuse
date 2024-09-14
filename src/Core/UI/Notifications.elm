module UI.Notifications exposing (Model, dismiss, show, showWithModel)

import Notifications exposing (..)
import Process
import Task
import UI.Types exposing (Msg(..))



-- 🌳


type alias Model =
    List (Notification Msg)



-- 📣


dismiss : Model -> { id : Int } -> ( Model, Cmd Msg )
dismiss collection { id } =
    ( List.map
        (\notification ->
            if Notifications.id notification == id then
                Notifications.dismiss notification

            else
                notification
        )
        collection
    , Task.perform
        (\_ -> RemoveNotification { id = id })
        (Process.sleep 500)
    )


show : Notification Msg -> Model -> ( Model, Cmd Msg )
show notification collection =
    let
        existingNotificationIds =
            List.map Notifications.id collection
    in
    if List.member (Notifications.id notification) existingNotificationIds then
        -- Don't show duplicate notifications
        ( collection
        , Cmd.none
        )

    else
        ( notification :: collection
          -- Hide notification after a certain amount of time,
          -- unless it's a sticky notification.
        , if (Notifications.options notification).sticky then
            Cmd.none

          else
            Task.perform
                (\_ -> DismissNotification { id = Notifications.id notification })
                (Process.sleep 7500)
        )


showWithModel : Model -> Notification Msg -> ( Model, Cmd Msg )
showWithModel model notification =
    show notification model
