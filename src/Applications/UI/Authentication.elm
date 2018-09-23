module UI.Authentication exposing (signInScreen)

import Authentication
import Color
import Html exposing (Html, a, button, div, em, img, span, text)
import Html.Attributes exposing (href, src, style, width)
import Html.Events exposing (onClick)
import Material.Icons.Action as Icons
import Tachyons exposing (classes)
import Tachyons.Classes as T
import UI.Chunky exposing (..)
import UI.Core
import UI.Kit



-- 🗺


signInScreen : Html UI.Core.Msg
signInScreen =
    chunk
        [ T.flex
        , T.flex_column
        , T.vh_100
        , T.items_center
        ]
        [ block
            [ style "height" "45%" ]
            [ T.flex
            , T.items_center
            ]
            [ img
                [ src "/images/diffuse-light.svg", width 190 ]
                []
            ]

        -----------------------------------------
        -- Choices
        -----------------------------------------
        , block
            []
            [ T.bg_white
            , T.br2
            , T.ph3
            , T.pv2
            ]
            (let
                borderStyle =
                    "1px solid " ++ Color.toCssString UI.Kit.colors.subtleBorder
             in
             [ -- Local
               button
                [ classes
                    [ T.b__none
                    , T.f6
                    , T.flex
                    , T.items_center
                    , T.lh_solid
                    , T.pointer
                    , T.pv3
                    , T.tl
                    ]

                --
                , style "background" "transparent"
                , style "min-width" "260px"

                --
                , onClick (UI.Core.SignIn Authentication.Local)
                ]
                [ slab
                    Html.span
                    []
                    [ T.inline_flex, T.mr3 ]
                    [ Icons.lock UI.Kit.colors.text 16 ]
                , text "Sign in anonymously"
                ]
             ]
            )

        -----------------------------------------
        -- Link to about page
        -----------------------------------------
        , chunk
            [ T.f6
            , T.flex
            , T.flex_grow_1
            , T.items_end
            , T.lh_solid
            , T.pb4
            ]
            [ a
                [ href "/about"
                , classes
                    [ T.no_underline
                    , T.white_50
                    ]
                ]
                [ em [] [ text "What is this exactly?" ] ]
            ]
        ]
