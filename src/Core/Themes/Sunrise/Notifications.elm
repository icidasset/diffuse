module Themes.Sunrise.Notifications exposing (view)

import Chunky exposing (..)
import Color exposing (Color)
import Color.Manipulate
import Conditional exposing (ifThenElse)
import Html exposing (Html, text)
import Html.Attributes exposing (class, rel, style)
import Html.Ext exposing (onDoubleTap, onTap)
import Html.Lazy
import Maybe.Extra as Maybe
import Notifications exposing (..)
import UI.Notifications exposing (Model)
import UI.Types exposing (Msg(..))



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
        |> chunk
            [ "notifications"

            --
            , "absolute"
            , "break-all"
            , "bottom-0"
            , "flex"
            , "flex-col"
            , "items-end"
            , "leading-snug"
            , "mb-4"
            , "mr-3"
            , "right-0"
            , "text-sm"
            , "z-50"
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
    brick
        [ if options.sticky then
            onDoubleTap dismissMsg

          else
            onTap dismissMsg

        --
        , rel (String.fromInt id)

        --
        , case kind of
            Casual ->
                Maybe.unwrap
                    (class "bg-white-20")
                    (style "background-color" << Color.toCssString)
                    extractedBackdropColor

            Error ->
                class "bg-base08"

            Success ->
                class "bg-base0b"
        ]
        [ "duration-200"
        , "max-w-xs"
        , "mt-2"
        , "p-4"
        , "rounded"
        , "text-white-90"

        --
        , ifThenElse options.sticky "cursor-pointer" "cursor-default"
        , ifThenElse options.sticky "select-none" "select-auto"

        --
        , if options.wasDismissed then
            "transition"

          else
            "transition-colors"

        --
        , if options.wasDismissed then
            "opacity-0"

          else
            "opacity-100"
        ]
        [ chunk
            [ "mt-px", "pt-px" ]
            [ contents notification ]

        --
        , if options.sticky && kind /= Casual then
            chunk
                [ "cursor-pointer"
                , "italic"
                , "mt-2"
                , "opacity-60"
                , "select-none"
                , "text-xs"
                ]
                [ text "Double click to dismiss" ]

          else
            nothing
        ]
