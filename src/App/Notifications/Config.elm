module Notifications.Config exposing (config)

import Html
import Html.Attributes
import Toasty
import Variables exposing (scaledStr)


config : Toasty.Config msg
config =
    Toasty.config
        |> Toasty.delay 3000
        |> Toasty.transitionOutDuration 475
        |> Toasty.containerAttrs containerAttrs
        |> Toasty.itemAttrs itemAttrs
        |> Toasty.transitionOutAttrs transitionOutAttrs


containerAttrs : List (Html.Attribute msg)
containerAttrs =
    [ Html.Attributes.style
        [ ( "bottom", scaledStr 1 )
        , ( "color", "white" )
        , ( "display", "block" )
        , ( "font-size", "0.8rem" )
        , ( "line-height", "1.4" )
        , ( "list-style", "none" )
        , ( "margin", "0" )
        , ( "max-width", "350px" )
        , ( "padding", "0" )
        , ( "position", "fixed" )
        , ( "right", scaledStr 1 )
        , ( "z-index", "1000000" )
        ]
    ]


itemAttrs : List (Html.Attribute msg)
itemAttrs =
    [ Html.Attributes.class "notification"
    , Html.Attributes.style
        [ ( "margin-top", scaledStr 1 )
        , ( "opacity", "0.00001" )
        , ( "overflow", "hidden" )
        , ( "transition", "opacity 475ms" )
        ]
    ]


transitionOutAttrs : List (Html.Attribute msg)
transitionOutAttrs =
    [ Html.Attributes.style
        [ ( "opacity", "0" )
        ]
    ]
