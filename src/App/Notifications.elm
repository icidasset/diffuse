module Notifications exposing (..)

import Notifications.Config exposing (config)
import Notifications.Types exposing (Notification)
import Ports
import Response.Ext as Response
import Toasty
import Types exposing (Model, Msg(ToastyMsg))


add : Notification -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
add notification grp =
    grp
        |> Toasty.addToast config ToastyMsg notification
        |> Response.addCmd (Ports.fadeInNotifications ())
