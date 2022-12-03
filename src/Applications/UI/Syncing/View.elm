module UI.Syncing.View exposing (view)

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
import Svg
import UI.Kit
import UI.Syncing.Types as Syncing exposing (..)
import UI.Types as UI exposing (..)
import User.Layer exposing (..)



-- 🗺


view : Model -> Html UI.Msg
view =
    Html.map SyncingMsg << Lazy.lazy view_ << .syncing


view_ : State -> Html Syncing.Msg
view_ state =
    case state of
        InputScreen _ opts ->
            UI.Kit.receptacle
                { scrolling = False }
                [ inputScreen opts ]

        NewEncryptionKeyScreen method pass ->
            UI.Kit.receptacle
                { scrolling = False }
                [ encryptionKeyScreen
                    { withEncryption = ActivateSyncWithPassphrase method (Maybe.withDefault "" pass)
                    , withoutEncryption = ActivateSync method
                    }
                ]

        UpdateEncryptionKeyScreen method pass ->
            UI.Kit.receptacle
                { scrolling = False }
                [ encryptionKeyScreen
                    { withEncryption = UpdateEncryptionKey method (Maybe.withDefault "" pass)
                    , withoutEncryption = RemoveEncryptionKey method
                    }
                ]

        Syncing ->
            Html.nothing

        Synced _ ->
            Html.nothing

        NotSynced ->
            Html.nothing



-- ENCRYPTION KEY


encryptionKeyScreen : { withEncryption : Syncing.Msg, withoutEncryption : Syncing.Msg } -> Html Syncing.Msg
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
            Syncing.Bypass
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


inputScreen : Question -> Html Syncing.Msg
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
            Syncing.Bypass
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
