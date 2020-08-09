module UI.Notifications exposing (Model, dismiss, show, showWithModel, view)

import Chunky exposing (..)
import Color exposing (Color)
import Color.Manipulate
import Conditional exposing (ifThenElse)
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (class, rel, style)
import Html.Ext exposing (onDoubleTap, onTap)
import Html.Lazy
import Maybe.Extra as Maybe
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



-- 🗺


view : Maybe Color -> Model -> Html Msg
view extractedBackdropColor collection =
    let
        manipulatedColor =
            Maybe.map
                (Color.Manipulate.darken 0.125)
                extractedBackdropColor
    in
    collection
        |> List.reverse
        |> List.map (Html.Lazy.lazy2 notificationView manipulatedColor)
        |> Html.div
            [ class "notifications"

            --
            , C.absolute
            , C.bottom_0
            , C.flex
            , C.flex_col
            , C.items_end
            , C.leading_snug
            , C.mb_4
            , C.mr_3
            , C.right_0
            , C.text_sm
            , C.z_50
            ]


notificationView : Maybe Color -> Notification Msg -> Html Msg
notificationView extractedBackdropColor notification =
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
    Html.div
        [ if options.sticky then
            onDoubleTap dismissMsg

          else
            onTap dismissMsg

        --
        , rel (String.fromInt id)

        --
        , C.duration_200
        , C.max_w_xs
        , C.mt_2
        , C.p_4
        , C.rounded
        , C.text_white_90

        --
        , ifThenElse options.sticky C.cursor_pointer C.cursor_default
        , ifThenElse options.sticky C.select_none C.select_auto

        --
        , case kind of
            Casual ->
                Maybe.unwrap
                    C.bg_white_20
                    (style "background-color" << Color.toCssString)
                    extractedBackdropColor

            Error ->
                C.bg_base08

            Success ->
                C.bg_base0b

        --
        , if options.wasDismissed then
            C.transition

          else
            C.transition_colors

        --
        , if options.wasDismissed then
            C.opacity_0

          else
            C.opacity_100
        ]
        [ Html.div
            [ C.mt_px, C.pt_px ]
            [ contents notification ]

        --
        , if options.sticky && kind /= Casual then
            Html.div
                [ C.cursor_pointer
                , C.italic
                , C.mt_2
                , C.opacity_60
                , C.select_none
                , C.text_xs
                ]
                [ text "Double click to dismiss" ]

          else
            nothing
        ]
