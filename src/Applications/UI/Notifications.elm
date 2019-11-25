module UI.Notifications exposing (Model, dismiss, show, showWithModel, view)

import Chunky.Styled exposing (..)
import Css
import Css.Classes as C
import Css.Global
import Html exposing (Html)
import Html.Ext exposing (onDoubleTap)
import Html.Styled exposing (text)
import Html.Styled.Attributes exposing (css, fromUnstyled, rel)
import Html.Styled.Lazy
import Notifications exposing (..)
import Process
import Task
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
    collection
        |> List.reverse
        |> List.map (Html.Styled.Lazy.lazy notificationView)
        |> brick
            [ css containerStyles ]
            [ C.absolute
            , C.bottom_0
            , C.flex
            , C.flex_col
            , C.items_end
            , C.leading_snug
            , C.mb_3
            , C.mr_3
            , C.right_0
            , C.text_sm
            , C.z_50
            ]
        |> Html.Styled.toUnstyled


notificationView : Notification Reply -> Html.Styled.Html Reply
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
        [ fromUnstyled (onDoubleTap dismissMsg)

        --
        , rel (String.fromInt id)
        ]
        [ C.max_w_xs
        , C.mt_2
        , C.p_4
        , C.rounded
        , C.text_white_90

        --
        , case kind of
            Error ->
                C.bg_base08

            Success ->
                C.bg_base0b

            Warning ->
                C.bg_white_20

        --
        , if options.wasDismissed then
            C.transition_250

          else
            ""

        --
        , if options.wasDismissed then
            C.opacity_0

          else
            C.opacity_100
        ]
        [ Html.Styled.fromUnstyled (contents notification)

        --
        , if options.sticky && kind /= Warning then
            chunk
                [ C.select_none
                , C.text_xs
                , C.italic
                , C.mt_2
                , C.opacity_60
                , C.cursor_pointer
                ]
                [ text "Double click to dismiss" ]

          else
            nothing
        ]



-- 🖼


containerStyles : List Css.Style
containerStyles =
    [ Css.fontSize (Css.px 13)
    , Css.lineHeight (Css.num 1.35)

    --
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
        , Css.Global.strong
            [ Css.fontWeight (Css.int 600)
            ]
        ]
    ]
