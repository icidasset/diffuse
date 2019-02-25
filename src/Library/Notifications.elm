module Notifications exposing (Action, Kind(..), Notification, Options, contents, error, id, kind, options, stickyError, success, warning)

import Chunky exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (class, style)
import Markdown
import Murmur3 exposing (..)
import Tachyons.Classes as T



-- 🌳


type Notification msg
    = Notification Kind Int Options (Html msg)


type alias Action msg =
    { label : String, msg : msg }


type alias Options =
    { sticky : Bool }


type Kind
    = Error
    | Success
    | Warning



-- 🔱


id : Notification msg -> Int
id (Notification _ i _ _) =
    i


contents : Notification msg -> Html msg
contents (Notification _ _ _ c) =
    c


kind : Notification msg -> Kind
kind (Notification k _ _ _) =
    k


options : Notification msg -> Options
options (Notification _ _ o _) =
    o



-- 🚨


error : String -> Notification msg
error content =
    Notification Error (hashString 0 content) { sticky = False } (render content)


stickyError : String -> String -> List (Action msg) -> Notification msg
stickyError content code actions =
    Notification
        Error
        (hashString 0 content)
        { sticky = True }
        (Html.div
            []
            [ render content
            , slab
                Html.pre
                [ style "font-size" "11px" ]
                [ T.bg_black_50, T.br2, T.mb0, T.mt3, T.pa2 ]
                [ Html.code [ class T.v_mid ] [ Html.text code ] ]
            ]
        )



-- 💚


success : String -> Notification msg
success content =
    Notification Success (hashString 0 content) { sticky = False } (render content)



-- ⚠️


warning : String -> Notification msg
warning content =
    Notification Warning (hashString 0 content) { sticky = False } (render content)



-- ⚗️


render : String -> Html msg
render content =
    content
        |> String.lines
        |> List.map String.trimLeft
        |> String.join "\n"
        |> Markdown.toHtml []
        |> Html.fromUnstyled
