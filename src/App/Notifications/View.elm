module Notifications.View exposing (entry)

import Color.Convert
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Markdown
import Notifications.Types exposing (Notification(..))
import Types exposing (Msg)
import Variables exposing (colors)


-- 🍯


entry : Notification -> Html Msg
entry notification =
    let
        color =
            case notification of
                Error _ ->
                    colors.base08

                Message _ ->
                    colors.base02

                Success _ ->
                    colors.base0B
    in
        div
            [ style
                [ ( "background-color", Color.Convert.colorToCssRgb color )
                , ( "border-radius", "3px" )
                , ( "padding", "0.25rem 1.25rem" )
                ]
            ]
            (case notification of
                Error str ->
                    Markdown.toHtml Nothing str

                Message str ->
                    Markdown.toHtml Nothing str

                Success str ->
                    Markdown.toHtml Nothing str
            )
