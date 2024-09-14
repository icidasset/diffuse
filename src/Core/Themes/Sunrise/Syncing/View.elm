module Themes.Sunrise.Syncing.View exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Html.Attributes as A exposing (attribute, placeholder, style, value)
import Html.Events as E exposing (onSubmit)
import Html.Events.Extra exposing (onClickStopPropagation)
import Html.Extra as Html
import Html.Lazy as Lazy
import Json.Decode
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import Themes.Sunrise.Kit as Kit
import UI.Syncing.Types as Syncing exposing (..)
import UI.Types as UI exposing (..)



-- 🗺


view : Model -> Html UI.Msg
view =
    Html.map SyncingMsg << Lazy.lazy view_ << .syncing


view_ : State -> Html Syncing.Msg
view_ state =
    case state of
        InputScreen _ opts ->
            Kit.receptacle
                { scrolling = False }
                [ inputScreen opts ]

        NewEncryptionKeyScreen method pass ->
            Kit.receptacle
                { scrolling = False }
                [ encryptionKeyScreen
                    { ableToCancel = False
                    , withEncryption = ActivateSyncWithPassphrase method (Maybe.withDefault "" pass)
                    , withoutEncryption = ActivateSync method
                    }
                ]

        UpdateEncryptionKeyScreen method pass ->
            Kit.receptacle
                { scrolling = False }
                [ encryptionKeyScreen
                    { ableToCancel = True
                    , withEncryption = UpdateEncryptionKey method (Maybe.withDefault "" pass)
                    , withoutEncryption = RemoveEncryptionKey method
                    }
                ]

        _ ->
            Html.nothing



-- ENCRYPTION KEY


encryptionKeyScreen : { ableToCancel : Bool, withEncryption : Syncing.Msg, withoutEncryption : Syncing.Msg } -> Html Syncing.Msg
encryptionKeyScreen { ableToCancel, withEncryption, withoutEncryption } =
    Kit.focusScreen
        { icon = Icons.lock
        , iconHref = Nothing
        , text = [ text "Optional passphrase to encrypt/decrypt your data" ]
        , textHref = Nothing
        }
        [ slab
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
            [ Kit.textArea
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
                , A.class "shadow"
                , E.onInput KeepPassphraseInMemory
                ]
            , chunk
                [ "flex"
                , "items-stretch"
                , "space-x-2.5"
                ]
                [ Kit.buttonWithOptions
                    Html.button
                    [ A.class "flex-1" ]
                    Kit.Gray
                    Kit.Filled
                    (Just Syncing.Bypass)
                    (text "Continue")

                --
                , if ableToCancel then
                    Kit.buttonWithOptions
                        Html.button
                        [ A.class "flex-1"
                        , E.stopPropagationOn "click" (Json.Decode.succeed ( CancelInput, True ))
                        ]
                        Kit.Gray
                        Kit.Normal
                        Nothing
                        (text "Cancel")

                  else
                    nothing
                ]
            , brick
                [ onClickStopPropagation withoutEncryption ]
                [ "cursor-pointer"
                , "flex"
                , "items-center"
                , "justify-center"
                , "leading-snug"
                , "mt-3"
                , "opacity-50"
                , "text-xs"
                ]
                [ inline [ "inline-block", "leading-none", "mr-2" ] [ Icons.warning 13 Inherit ]
                , text "Continue without encryption"
                ]
            ]
        ]



-- INPUT SCREEN


inputScreen : Question -> Html Syncing.Msg
inputScreen question =
    Kit.focusScreen
        { icon = question.icon
        , iconHref = Nothing
        , text = [ Kit.askForInput question.question ]
        , textHref = Nothing
        }
        [ slab
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
            [ Kit.textFieldAlt
                [ attribute "autocapitalize" "off"
                , placeholder question.placeholder
                , A.class "shadow"
                , E.onInput Input
                , value question.value
                ]
            , Kit.button
                Kit.Filled
                Syncing.Bypass
                (text "Continue")
            ]
        ]



-- SPEECH BUBBLE
-- 🖼
