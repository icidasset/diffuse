module Notifications.Config exposing (config)

import Html
import Html.Attributes
import Toasty
import Traits exposing (grs)


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
        [ ( "bottom", grs 2 )
        , ( "color", "white" )
        , ( "display", "block" )
        , ( "font-size", "0.8rem" )
        , ( "line-height", "1.4" )
        , ( "list-style", "none" )
        , ( "margin", "0" )
        , ( "max-width", grs 55 )
        , ( "padding", "0" )
        , ( "position", "fixed" )
        , ( "right", grs 2 )
        , ( "z-index", "1000000" )
        ]
    ]


itemAttrs : List (Html.Attribute msg)
itemAttrs =
    [ Html.Attributes.class "notification"
    , Html.Attributes.style
        [ ( "margin-top", grs 2 )
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
