module UI.Authentication.View exposing (view)

import Chunky exposing (..)
import Conditional exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (attribute, placeholder, style, value)
import Html.Events as E exposing (onSubmit)
import Html.Events.Extra exposing (onClickStopPropagation)
import Html.Extra as Html
import Html.Lazy as Lazy
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import Svg exposing (Svg)
import UI.Authentication.Types as Authentication exposing (..)
import UI.Kit
import UI.Types as UI exposing (..)
import User.Layer exposing (..)



-- 🗺


view : Model -> Html UI.Msg
view =
    Html.map AuthenticationMsg << Lazy.lazy view_ << .authentication


view_ : State -> Html Authentication.Msg
view_ state =
    case state of
        InputScreen _ opts ->
            inputScreen opts

        NewEncryptionKeyScreen method pass ->
            encryptionKeyScreen
                { withEncryption = SignInWithPassphrase method (Maybe.withDefault "" pass)
                , withoutEncryption = SignIn method
                }

        UpdateEncryptionKeyScreen method pass ->
            encryptionKeyScreen
                { withEncryption = UpdateEncryptionKey method (Maybe.withDefault "" pass)
                , withoutEncryption = RemoveEncryptionKey method
                }

        Syncing ->
            Html.nothing

        Synced _ ->
            Html.nothing

        NotSynced ->
            Html.nothing



-- ENCRYPTION KEY


encryptionKeyScreen : { withEncryption : Authentication.Msg, withoutEncryption : Authentication.Msg } -> Html Authentication.Msg
encryptionKeyScreen { withEncryption, withoutEncryption } =
    slab
        Html.form
        [ onSubmit withEncryption ]
        [ "flex"
        , "flex-col"
        , "max-w-xs"
        , "px-3"
        , "w-screen"

        --
        , "sm:px-0"
        ]
        [ UI.Kit.textArea
            [ attribute "autocapitalize" "none"
            , attribute "autocomplete" "off"
            , attribute "autocorrect" "off"
            , attribute "rows" "4"
            , attribute "spellcheck" "false"

            --
            , placeholder "anQLS9Usw24gxUi11IgVBg76z8SCWZgLKkoWIeJ1ClVmBHLRlaiA0CtvONVAMGritbgd3U45cPTxrhFU0WXaOAa8pVt186KyEccfUNyAq97"

            --
            , style "-webkit-text-security" "disc"

            --
            , E.onInput KeepPassphraseInMemory
            ]
        , UI.Kit.button
            UI.Kit.Filled
            Authentication.Bypass
            (text "Continue")
        , brick
            [ onClickStopPropagation withoutEncryption ]
            [ "cursor-pointer"
            , "flex"
            , "items-center"
            , "justify-center"
            , "leading-snug"
            , "mt-3"
            , "opacity-50"
            , "text-white"
            , "text-xs"
            ]
            [ inline [ "inline-block", "leading-none", "mr-2" ] [ Icons.warning 13 Inherit ]
            , text "Continue without encryption"
            ]
        ]



-- INPUT SCREEN


inputScreen : Question -> Html Authentication.Msg
inputScreen question =
    slab
        Html.form
        [ onSubmit ConfirmInput ]
        [ "flex"
        , "flex-col"
        , "max-w-xs"
        , "px-3"
        , "w-screen"

        --
        , "sm:px-0"
        ]
        [ UI.Kit.textFieldAlt
            [ attribute "autocapitalize" "off"
            , placeholder question.placeholder
            , E.onInput Input
            , value question.value
            ]
        , UI.Kit.button
            UI.Kit.Filled
            Authentication.Bypass
            (text "Continue")
        ]



-- SPEECH BUBBLE


speechBubble : Html msg -> Html msg
speechBubble contents =
    chunk
        [ "absolute"
        , "antialiased"
        , "bg-background"
        , "border-b"
        , "border-transparent"
        , "font-semibold"
        , "italic"
        , "leading-snug"
        , "left-1/2"
        , "max-w-screen"
        , "-translate-x-1/2"
        , "px-4"
        , "py-1"
        , "rounded"
        , "text-center"
        , "text-sm"
        , "text-white"
        , "top-full"
        , "transform"
        , "whitespace-nowrap"

        -- Dark mode
        ------------
        , "dark:bg-darkest-hour"
        , "dark:text-gray-600"
        ]
        [ chunk
            [ "mb-px", "pb-px", "pt-1" ]
            [ contents ]

        --
        , brick
            speechBubbleArrowStyles
            [ "absolute"
            , "border-background"
            , "h-0"
            , "left-1/2"
            , "-translate-x-1/2"
            , "-translate-y-full"
            , "top-0"
            , "transform"
            , "w-0"

            -- Dark mode
            ------------
            , "dark:border-darkest-hour"
            ]
            []
        ]



-- 🖼


speechBubbleArrowStyles : List (Html.Attribute msg)
speechBubbleArrowStyles =
    [ style "border-top-color" "transparent"
    , style "border-left-color" "transparent"
    , style "border-right-color" "transparent"
    , style "border-width" "0 6px 5px 6px"
    ]
