module UI.Authentication exposing (Model, Msg(..), initialModel, update, view)

import Alien
import Authentication exposing (Method(..))
import Chunky exposing (..)
import Color exposing (Color)
import Color.Ext as Color
import Common exposing (Switch(..))
import Conditional exposing (..)
import Crypto.Hash
import Css exposing (pct, px, solid, transparent)
import Html.Styled as Html exposing (Html, a, button, div, em, fromUnstyled, img, span, text)
import Html.Styled.Attributes exposing (attribute, css, href, placeholder, src, style, type_, width)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Json
import Json.Encode
import Material.Icons.Action as Icons
import Replying exposing (R3D3)
import Return2 as R2
import Return3 as R3
import Svg exposing (Svg)
import Tachyons.Classes as T
import UI.Kit
import UI.Ports as Ports
import UI.Reply exposing (Reply(..))



-- 🌳


type alias Model =
    { encryptionKeyInputFor : Maybe Authentication.Method
    , methodInUse : Maybe Authentication.Method
    }


initialModel : Model
initialModel =
    { encryptionKeyInputFor = Nothing
    , methodInUse = Nothing
    }



-- 📣


type Msg
    = Bypass
    | SignIn Authentication.Method
      -----------------------------------------
      -- Method
      -----------------------------------------
    | ActivateMethod Json.Value
    | DischargeMethod
    | FabricateSecretKey String
      -----------------------------------------
      -- Private Key
      -----------------------------------------
    | HideEncryptionKeyScreen
    | ShowEncryptionKeyScreen Authentication.Method


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            R3.withNothing model

        SignIn method ->
            ( { model | encryptionKeyInputFor = Nothing }
            , method
                |> Authentication.methodToString
                |> Json.Encode.string
                |> Alien.broadcast Alien.SignIn
                |> Ports.toBrain
            , Just [ ToggleLoadingScreen On ]
            )

        -----------------------------------------
        -- Method
        -----------------------------------------
        ActivateMethod encodedMethod ->
            R3.withNothing { model | methodInUse = Authentication.decodeMethod encodedMethod }

        DischargeMethod ->
            R3.withNothing { model | methodInUse = Nothing }

        FabricateSecretKey passphrase ->
            [ ( "tag", Json.Encode.string <| Alien.tagToString Alien.AuthSecretKey )
            , ( "data", Json.Encode.string <| Crypto.Hash.sha256 passphrase )
            , ( "error", Json.Encode.null )
            ]
                |> Json.Encode.object
                |> Alien.broadcast Alien.ToCache
                |> Ports.toBrain
                |> R2.withModel model
                |> R3.withNoReply

        -----------------------------------------
        -- Private Key
        -----------------------------------------
        HideEncryptionKeyScreen ->
            R3.withNothing { model | encryptionKeyInputFor = Nothing }

        ShowEncryptionKeyScreen method ->
            R3.withNothing { model | encryptionKeyInputFor = Just method }



-- 🗺


view : Model -> Html Msg
view model =
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
            [ chunk
                [ T.relative ]
                [ img
                    [ onClick HideEncryptionKeyScreen
                    , src "/images/diffuse-light.svg"
                    , width 190

                    --
                    , case model.encryptionKeyInputFor of
                        Just _ ->
                            style "cursor" "pointer"

                        Nothing ->
                            style "cursor" "default"
                    ]
                    []

                -- Speech bubble
                ----------------
                , case model.encryptionKeyInputFor of
                    Just _ ->
                        [ text "I need a passphrase"
                        , lineBreak
                        , text "to encrypt your data."
                        ]
                            |> chunk []
                            |> speechBubble

                    Nothing ->
                        nothing
                ]
            ]

        -----------------------------------------
        -- Content
        -----------------------------------------
        , case model.encryptionKeyInputFor of
            Just method ->
                chunk
                    [ T.flex
                    , T.flex_column
                    ]
                    [ UI.Kit.textArea
                        [ attribute "autocomplete" "off"
                        , attribute "autocorrect" "off"
                        , attribute "spellcheck" "false"
                        , placeholder "anQLS9Usw24gxUi11IgVBg76z8SCWZgLKkoWIeJ1ClVmBHLRlaiA0CtvONVAMGritbgd3U45cPTxrhFU0WXaOAa8pVt186KyEccfUNyAq97"
                        , Html.Styled.Events.onInput FabricateSecretKey
                        ]
                    , UI.Kit.button
                        UI.Kit.Normal
                        (SignIn method)
                        (text "Continue")
                    ]

            Nothing ->
                chunk
                    [ T.bg_white
                    , T.br2
                    , T.ph3
                    , T.pv2
                    ]
                    [ choiceButton
                        { action = SignIn Authentication.Local
                        , icon = Icons.lock_open
                        , isLast = False
                        , label = "Store data in the browser"
                        }
                    , choiceButton
                        { action = ShowEncryptionKeyScreen Authentication.Ipfs
                        , icon = Icons.fingerprint
                        , isLast = True
                        , label = "Store encrypted data on IPFS"
                        }
                    ]

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
            , T.pt3
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


choiceButton :
    { action : msg
    , icon : Color -> Int -> Svg msg
    , isLast : Bool
    , label : String
    }
    -> Html msg
choiceButton { action, icon, isLast, label } =
    slab
        button
        [ css (choiceButtonStyles { border = not isLast })
        , onClick action
        ]
        [ T.b__none
        , T.f6
        , T.flex
        , T.items_center
        , T.lh_solid
        , T.outline_0
        , T.pointer
        , T.pv3
        , T.tl
        ]
        [ slab
            span
            []
            [ T.inline_flex, T.mr3 ]
            [ fromUnstyled (icon UI.Kit.colors.text 16) ]
        , text label
        ]


speechBubble : Html msg -> Html msg
speechBubble contents =
    brick
        [ css speechBubbleStyles ]
        [ T.absolute
        , T.br2
        , T.f6
        , T.lh_copy
        , T.mt3
        , T.nowrap
        , T.ph3
        , T.pv2
        , T.tc
        , T.white
        ]
        [ contents

        --
        , brick
            [ css speechBubbleArrowStyles ]
            [ T.absolute, T.top_0 ]
            []
        ]



-- 🖼


choiceButtonStyles : { border : Bool } -> List Css.Style
choiceButtonStyles { border } =
    [ Css.backgroundColor transparent
    , Css.borderBottom3
        (ifThenElse border (px 1) (px 0))
        solid
        (Color.toElmCssColor UI.Kit.colors.subtleBorder)
    , Css.minWidth (px 260)
    ]


speechBubbleStyles : List Css.Style
speechBubbleStyles =
    [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colors.background)
    , Css.borderBottom3 (px 1) solid transparent
    , Css.left (pct 50)
    , Css.lineHeight (Css.num 1.4)
    , Css.top (pct 100)
    , Css.transform (Css.translateX <| pct -50)
    ]


speechBubbleArrowStyles : List Css.Style
speechBubbleArrowStyles =
    let
        color =
            Color.toCssString UI.Kit.colors.background
    in
    [ Css.height (px 0)
    , Css.left (pct 50)
    , Css.transform (Css.translate2 (pct -50) (pct -100))
    , Css.width (px 0)

    --
    , Css.property "border-color" ("transparent transparent " ++ color ++ " transparent")
    , Css.property "border-style" "solid"
    , Css.property "border-width" "0 6px 5px 6px"
    ]
