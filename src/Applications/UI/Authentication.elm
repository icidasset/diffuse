module UI.Authentication exposing (signInScreen)

import Authentication
import Chunky exposing (..)
import Color
import Css exposing (backgroundColor, minWidth, px, transparent)
import Html.Styled as Html exposing (Html, a, button, div, em, fromUnstyled, img, span, text)
import Html.Styled.Attributes exposing (css, href, src, style, width)
import Html.Styled.Events exposing (onClick)
import Material.Icons.Action as Icons
import Tachyons.Classes as T
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
        [ brick
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
        , brick
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
               --------
               slab
                button
                [ css choiceButtonStyles
                , onClick (UI.Core.SignIn Authentication.Local)
                ]
                [ T.b__none
                , T.f6
                , T.flex
                , T.items_center
                , T.lh_solid
                , T.pointer
                , T.pv3
                , T.tl
                ]
                [ slab
                    span
                    []
                    [ T.inline_flex, T.mr3 ]
                    [ fromUnstyled (Icons.lock UI.Kit.colors.text 16) ]
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
            [ slab
                a
                [ href "/about" ]
                [ T.no_underline
                , T.white_50
                ]
                [ em [] [ text "What is this exactly?" ] ]
            ]
        ]



-- 🖼


choiceButtonStyles : List Css.Style
choiceButtonStyles =
    [ backgroundColor transparent
    , minWidth (px 260)
    ]
