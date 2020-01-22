module Notifications exposing (Action, Kind(..), Notification, Options, contents, dismiss, error, errorWithCode, id, kind, options, stickyError, stickySuccess, stickyWarning, success, warning)

import Chunky exposing (..)
import Css.Classes as C
import Html exposing (Html)
import Markdown
import Murmur3 exposing (..)



-- 🌳


type Notification msg
    = Notification Kind Int Options (Html msg)


type alias Action msg =
    { label : String, msg : msg }


type alias Options =
    { sticky : Bool, wasDismissed : Bool }


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



-- ⚗️


dismiss : Notification msg -> Notification msg
dismiss (Notification k i o c) =
    Notification k i { o | wasDismissed = True } c



-- 🚨


error : String -> Notification msg
error content =
    Notification
        Error
        (hashString 0 content)
        { sticky = False
        , wasDismissed = False
        }
        (render content)


stickyError : String -> Notification msg
stickyError content =
    Notification
        Error
        (hashString 0 content)
        { sticky = True
        , wasDismissed = False
        }
        (render content)


errorWithCode : String -> String -> List (Action msg) -> Notification msg
errorWithCode content code _ =
    Notification
        Error
        (hashString 0 content)
        { sticky = True
        , wasDismissed = False
        }
        (Html.div
            []
            [ render content
            , if String.isEmpty (String.trim code) then
                nothing

              else
                chunk
                    [ C.bg_black_50
                    , C.break_all
                    , C.rounded
                    , C.mb_0
                    , C.mt_3
                    , C.p_2
                    , C.text_xxs
                    ]
                    [ slab
                        Html.code
                        []
                        [ C.align_middle ]
                        [ Html.text code ]
                    ]
            ]
        )



-- 💚


success : String -> Notification msg
success content =
    Notification
        Success
        (hashString 0 content)
        { sticky = False
        , wasDismissed = False
        }
        (render content)


stickySuccess : String -> Notification msg
stickySuccess content =
    Notification
        Success
        (hashString 0 content)
        { sticky = True
        , wasDismissed = False
        }
        (render content)



-- ⚠️


warning : String -> Notification msg
warning content =
    Notification
        Warning
        (hashString 0 content)
        { sticky = False
        , wasDismissed = False
        }
        (render content)


stickyWarning : String -> Notification msg
stickyWarning content =
    Notification
        Warning
        (hashString 0 content)
        { sticky = True
        , wasDismissed = False
        }
        (render content)



-- ⚗️


render : String -> Html msg
render content =
    content
        |> String.lines
        |> List.map String.trimLeft
        |> String.join "\n"
        |> Markdown.toHtml []
