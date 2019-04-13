module UI.Authentication exposing (Model(..), Msg(..), extractMethod, initialModel, update, view)

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
import Html.Styled.Attributes exposing (attribute, css, href, placeholder, src, style, type_, value, width)
import Html.Styled.Events exposing (onClick, onSubmit)
import Json.Decode as Json
import Json.Encode
import Material.Icons.Av as Icons
import Replying exposing (R3D3, andThen3)
import Return2 as R2
import Return3 as R3
import Svg exposing (Svg)
import Tachyons.Classes as T
import UI.Kit
import UI.Ports as Ports
import UI.Reply exposing (Reply(..))
import UI.Svg.Elements



-- ⛩


minimumPassphraseLength =
    16


passphraseLengthErrorMessage =
    "Your passphrase should be atleast *16 characters* long."



-- 🌳


type Model
    = Authenticated Method
    | NewEncryptionKeyScreen Method (Maybe String)
    | UpdateEncryptionKeyScreen Method (Maybe String)
    | Unauthenticated


initialModel : Model
initialModel =
    Unauthenticated


extractMethod : Model -> Maybe Method
extractMethod model =
    case model of
        Authenticated method ->
            Just method

        NewEncryptionKeyScreen method _ ->
            Just method

        UpdateEncryptionKeyScreen method _ ->
            Just method

        Unauthenticated ->
            Nothing



-- 📣


type Msg
    = Bypass
    | HideEncryptionKeyScreen
    | KeepPassphraseInMemory String
    | ShowNewEncryptionKeyScreen Method
    | ShowUpdateEncryptionKeyScreen Method
    | SignIn Method
    | SignInWithPassphrase Method String
    | SignedIn Method
    | UpdateEncryptionKey Method String


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            R3.withNothing model

        HideEncryptionKeyScreen ->
            case model of
                Authenticated method ->
                    R3.withNothing (Authenticated method)

                NewEncryptionKeyScreen _ _ ->
                    R3.withNothing Unauthenticated

                UpdateEncryptionKeyScreen method _ ->
                    R3.withNothing (Authenticated method)

                Unauthenticated ->
                    R3.withNothing Unauthenticated

        KeepPassphraseInMemory passphrase ->
            case model of
                NewEncryptionKeyScreen method _ ->
                    R3.withNothing (NewEncryptionKeyScreen method <| Just passphrase)

                UpdateEncryptionKeyScreen method _ ->
                    R3.withNothing (UpdateEncryptionKeyScreen method <| Just passphrase)

                _ ->
                    R3.withNothing model

        ShowNewEncryptionKeyScreen method ->
            R3.withNothing (NewEncryptionKeyScreen method Nothing)

        ShowUpdateEncryptionKeyScreen method ->
            R3.withNothing (UpdateEncryptionKeyScreen method Nothing)

        SignIn method ->
            ( Unauthenticated
            , signIn method
            , Just [ ToggleLoadingScreen On ]
            )

        SignInWithPassphrase method passphrase ->
            if String.length passphrase < minimumPassphraseLength then
                ( model
                , Cmd.none
                , Just [ ShowErrorNotification passphraseLengthErrorMessage ]
                )

            else
                ( Unauthenticated
                , signInWithPassphrase method passphrase
                , Just [ ToggleLoadingScreen On ]
                )

        SignedIn method ->
            R3.withNothing (Authenticated method)

        UpdateEncryptionKey method passphrase ->
            if String.length passphrase < minimumPassphraseLength then
                ( model
                , Cmd.none
                , Just [ ShowErrorNotification passphraseLengthErrorMessage ]
                )

            else
                ( Authenticated method
                , passphrase
                    |> Crypto.Hash.sha256
                    |> Json.Encode.string
                    |> Alien.broadcast Alien.UpdateEncryptionKey
                    |> Ports.toBrain
                , Nothing
                )


signIn : Method -> Cmd Msg
signIn method =
    [ ( "method", Authentication.encodeMethod method )
    , ( "passphrase", Json.Encode.null )
    ]
        |> Json.Encode.object
        |> Alien.broadcast Alien.SignIn
        |> Ports.toBrain


signInWithPassphrase : Method -> String -> Cmd Msg
signInWithPassphrase method passphrase =
    [ ( "method", Authentication.encodeMethod method )
    , ( "passphrase", Json.Encode.string <| Crypto.Hash.sha256 passphrase )
    ]
        |> Json.Encode.object
        |> Alien.broadcast Alien.SignIn
        |> Ports.toBrain



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
            [ style "height" "42%" ]
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
                    , case model of
                        NewEncryptionKeyScreen _ _ ->
                            style "cursor" "pointer"

                        UpdateEncryptionKeyScreen _ _ ->
                            style "cursor" "pointer"

                        _ ->
                            style "cursor" "default"
                    ]
                    []

                -- Speech bubble
                ----------------
                , case model of
                    NewEncryptionKeyScreen _ _ ->
                        [ text "I need a passphrase"
                        , lineBreak
                        , text "to encrypt your data."
                        ]
                            |> chunk []
                            |> speechBubble

                    UpdateEncryptionKeyScreen _ _ ->
                        [ text "I need a new passphrase"
                        , lineBreak
                        , text "to encrypt your data."
                        ]
                            |> chunk []
                            |> speechBubble

                    _ ->
                        [ text "Where would you like to"
                        , lineBreak
                        , text "store your encrypted data?"
                        ]
                            |> chunk []
                            |> speechBubble
                ]
            ]

        -----------------------------------------
        -- Content
        -----------------------------------------
        , case model of
            NewEncryptionKeyScreen method Nothing ->
                encryptionKeyScreen Bypass

            NewEncryptionKeyScreen method (Just passphrase) ->
                encryptionKeyScreen (SignInWithPassphrase method passphrase)

            UpdateEncryptionKeyScreen method Nothing ->
                encryptionKeyScreen Bypass

            UpdateEncryptionKeyScreen method (Just passphrase) ->
                encryptionKeyScreen (UpdateEncryptionKey method passphrase)

            Unauthenticated ->
                choicesScreen

            Authenticated _ ->
                choicesScreen

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



-- CHOICES


encryptionKeyScreen : Msg -> Html Msg
encryptionKeyScreen msg =
    chunk
        [ T.flex
        , T.flex_column
        ]
        [ UI.Kit.textArea
            [ attribute "autocomplete" "off"
            , attribute "autocorrect" "off"
            , attribute "spellcheck" "false"
            , placeholder "anQLS9Usw24gxUi11IgVBg76z8SCWZgLKkoWIeJ1ClVmBHLRlaiA0CtvONVAMGritbgd3U45cPTxrhFU0WXaOAa8pVt186KyEccfUNyAq97"
            , Html.Styled.Events.onInput KeepPassphraseInMemory
            ]
        , UI.Kit.button
            UI.Kit.Normal
            msg
            (text "Continue")
        ]


choicesScreen : Html Msg
choicesScreen =
    chunk
        [ T.bg_white
        , T.br2
        , T.ph3
        , T.pv2
        ]
        [ choiceButton
            { action = ShowNewEncryptionKeyScreen Authentication.Local
            , icon = Icons.web
            , isLast = False
            , label = "My Browser"
            , outOfOrder = False
            }
        , choiceButton
            { action = Bypass
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.blockstackLogo
            , isLast = False
            , label = "Blockstack"
            , outOfOrder = True
            }
        , choiceButton
            { action = ShowNewEncryptionKeyScreen Authentication.Ipfs
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.ipfsLogo
            , isLast = False
            , label = "IPFS"
            , outOfOrder = False
            }
        , choiceButton
            { action = Bypass
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.remoteStorageLogo
            , isLast = False
            , label = "RemoteStorage"
            , outOfOrder = True
            }
        , choiceButton
            { action = Bypass
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.solidLogo
            , isLast = True
            , label = "Solid"
            , outOfOrder = True
            }
        ]


choiceButton :
    { action : msg
    , icon : Color -> Int -> Svg msg
    , isLast : Bool
    , label : String
    , outOfOrder : Bool
    }
    -> Html msg
choiceButton { action, icon, isLast, label, outOfOrder } =
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
        [ -- TODO: Remove `chunk` + outOfOrder when everything is implemented
          chunk
            [ T.flex
            , T.items_center

            --
            , ifThenElse outOfOrder T.o_20 T.o_100
            ]
            [ slab
                span
                []
                [ T.inline_flex, T.mr3 ]
                [ fromUnstyled (icon UI.Kit.colors.text 16) ]
            , text label
            ]
        ]



-- ENCRYPTION KEY


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
    , Css.minWidth (px 210)
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
