module UI.Notifications exposing (Model, dismiss, show, showWithModel, view)

import Chunky exposing (..)
import Classes as C
import Color.Ext as Color
import Css
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, rel)
import Html.Styled.Events exposing (onDoubleClick)
import Html.Styled.Ext exposing (onDoubleTap)
import Html.Styled.Lazy
import Notifications exposing (..)
import Process
import Tachyons.Classes as T
import Task
import UI.Kit
import UI.Reply exposing (Reply(..))



-- 🌳


type alias Model =
    List (Notification Reply)



-- 📣


dismiss : Model -> { id : Int } -> ( Model, Cmd Reply )
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


show : Notification Reply -> Model -> ( Model, Cmd Reply )
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


showWithModel : Model -> Notification Reply -> ( Model, Cmd Reply )
showWithModel model notification =
    show notification model



-- 🗺


view : Model -> Html Reply
view collection =
    brick
        [ css containerStyles ]
        [ T.absolute
        , T.bottom_0
        , T.f6
        , T.hide_child
        , T.lh_title
        , T.mb3
        , T.mr3
        , T.right_0
        , T.z_9999
        ]
        (List.map
            (Html.Styled.Lazy.lazy notificationView)
            (List.reverse collection)
        )


notificationView : Notification Reply -> Html Reply
notificationView notification =
    let
        kind =
            Notifications.kind notification

        options =
            Notifications.options notification

        id =
            Notifications.id notification

        dismissMsg =
            DismissNotification { id = id }
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
        , onDoubleClick dismissMsg
        , onDoubleTap dismissMsg

        --
        , rel (String.fromInt id)
        ]
        [ T.br2
        , T.cb
        , T.fr
        , T.measure_narrow
        , T.mt2
        , T.pa3
        , T.white_90

        --
        , if options.wasDismissed then
            T.child

          else
            ""

        --
        , if options.wasDismissed then
            T.o_0

          else
            T.o_100
        ]
        [ contents notification
        , if options.sticky && kind /= Warning then
            chunk
                [ C.disable_selection, T.f7, T.i, T.mt2, T.o_60, T.pointer ]
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
        [ Css.Global.a
            [ Css.borderBottom3 (Css.px 1) Css.solid (Css.rgba 255 255 255 0.45)
            , Css.color Css.inherit
            , Css.display Css.inlineBlock
            , Css.fontWeight (Css.int 600)
            , Css.textDecoration Css.none
            ]
        , Css.Global.p
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
    [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colors.error) ]


successStyles : List Css.Style
successStyles =
    [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colors.success) ]


warningStyles : List Css.Style
warningStyles =
    [ Css.backgroundColor (Css.rgba 255 255 255 0.2) ]
