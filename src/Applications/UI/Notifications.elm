module UI.Notifications exposing (dismiss, show, showWithModel, view)

import Chunky exposing (..)
import Color.Ext as Color
import Css
import Css.Ext as Css
import Css.Global
import Css.Transitions exposing (transition)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, rel)
import Html.Styled.Events exposing (onDoubleClick)
import Html.Styled.Lazy
import Notifications exposing (..)
import Process
import Tachyons.Classes as T
import Task
import UI.Core exposing (Model, Msg)
import UI.Kit



-- 📣


dismiss : Model -> { id : Int } -> ( Model, Cmd Msg )
dismiss model { id } =
    ( { model
        | notifications =
            List.map
                (\notification ->
                    if Notifications.id notification == id then
                        Notifications.dismiss notification

                    else
                        notification
                )
                model.notifications
      }
    , Task.perform
        (\_ -> UI.Core.RemoveNotification { id = id })
        (Process.sleep 500)
    )


show : Notification Msg -> Model -> ( Model, Cmd Msg )
show notification model =
    let
        existingNotificationIds =
            List.map Notifications.id model.notifications
    in
    if List.member (Notifications.id notification) existingNotificationIds then
        -- Don't show duplicate notifications
        ( model
        , Cmd.none
        )

    else
        ( { model | notifications = notification :: model.notifications }
          -- Hide notification after a certain amount of time,
          -- unless it's a sticky notification.
        , if (Notifications.options notification).sticky then
            Cmd.none

          else
            Task.perform
                (\_ -> UI.Core.DismissNotification { id = Notifications.id notification })
                (Process.sleep 3000)
        )


showWithModel : Model -> Notification Msg -> ( Model, Cmd Msg )
showWithModel model notification =
    show notification model



-- 🗺


view : List (Notification Msg) -> Html Msg
view notifications =
    brick
        [ css containerStyles ]
        [ T.absolute
        , T.bottom_0
        , T.f6
        , T.lh_title
        , T.mb3
        , T.mr3
        , T.right_0
        , T.z_9999
        ]
        (List.map
            (Html.Styled.Lazy.lazy notificationView)
            (List.reverse notifications)
        )


notificationView : Notification Msg -> Html Msg
notificationView notification =
    let
        kind =
            Notifications.kind notification

        options =
            Notifications.options notification
    in
    brick
        [ case kind of
            Error ->
                css errorStyles

            Success ->
                css successStyles

            Warning ->
                css warningStyles

        --
        , notification
            |> Notifications.id
            |> (\id -> { id = id })
            |> UI.Core.DismissNotification
            |> onDoubleClick
        ]
        [ T.br2
        , T.mt2
        , T.pa3
        , T.white_90

        --
        , case kind of
            Error ->
                T.measure_narrow

            Success ->
                T.measure_wide

            Warning ->
                ""

        --
        , if options.wasDismissed then
            T.o_0

          else
            T.o_100
        ]
        [ contents notification
        , if options.sticky && kind /= Warning then
            brick
                [ css [ Css.disableUserSelection ] ]
                [ T.f7, T.i, T.mt2, T.o_60, T.pointer ]
                [ Html.text "Double click to dismiss" ]

          else
            nothing
        ]



-- 🖼


containerStyles : List Css.Style
containerStyles =
    [ Css.fontSize (Css.px 13)
    , Css.lineHeight (Css.num 1.35)
    , Css.Global.descendants
        [ Css.Global.p
            [ Css.margin Css.zero
            , Css.padding Css.zero
            ]
        , Css.Global.em
            [ Css.borderBottom3 (Css.px 1) Css.solid (Css.rgba 255 255 255 0.45)
            , Css.fontWeight Css.inherit
            ]
        ]
    ]


errorStyles : List Css.Style
errorStyles =
    [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colors.error)
    , Css.Transitions.transition [ Css.Transitions.opacity 450 ]
    ]


successStyles : List Css.Style
successStyles =
    [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colors.success)
    , Css.Transitions.transition [ Css.Transitions.opacity 450 ]
    ]


warningStyles : List Css.Style
warningStyles =
    [ Css.backgroundColor (Css.rgba 255 255 255 0.2)
    , Css.Transitions.transition [ Css.Transitions.opacity 450 ]
    ]
