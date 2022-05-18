module UI.Authentication.View exposing (view)

import Chunky exposing (..)
import Conditional exposing (..)
import Html exposing (Html, a, button, text)
import Html.Attributes exposing (attribute, href, placeholder, src, style, target, title, value, width)
import Html.Events exposing (onClick, onSubmit)
import Html.Events.Extra exposing (onClickStopPropagation)
import Html.Events.Extra.Mouse as Mouse
import Html.Extra as Html
import Html.Lazy as Lazy
import Markdown
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import Svg exposing (Svg)
import UI.Authentication.Types as Authentication exposing (..)
import UI.Kit
import UI.Svg.Elements
import UI.Types as UI exposing (..)
import User.Layer exposing (..)



-- 🗺


view : Model -> Html UI.Msg
view =
    Html.map AuthenticationMsg << Lazy.lazy view_ << .authentication


view_ : State -> Html Authentication.Msg
view_ state =
    chunk
        [ "flex"
        , "flex-col"
        , "h-full"
        , "items-center"
        ]
        [ brick
            [ style "height" "42%" ]
            [ "flex"
            , "items-center"
            , "pb-8"

            --
            , "md:pb-0"
            ]
            [ -- Logo
              -------
              chunk
                [ "py-5", "relative" ]
                [ slab
                    Html.img
                    [ onClick CancelFlow
                    , src "images/diffuse-light.svg"
                    , width 190

                    --
                    , case state of
                        Welcome ->
                            title "Diffuse"

                        _ ->
                            title "Go back"
                    ]
                    [ case state of
                        Welcome ->
                            "cursor-default"

                        _ ->
                            "cursor-pointer"
                    ]
                    []

                -- Speech bubble
                ----------------
                , case state of
                    Authenticating ->
                        speechBubble negotiating

                    InputScreen _ { question } ->
                        question
                            |> String.lines
                            |> List.map String.trimLeft
                            |> String.join "\n"
                            |> Markdown.toHtmlWith
                                { githubFlavored = Nothing
                                , defaultHighlighting = Nothing
                                , sanitize = False
                                , smartypants = True
                                }
                                []
                            |> speechBubble

                    NewEncryptionKeyScreen _ _ ->
                        [ text "I need a passphrase to encrypt your personal data."
                        , lineBreak
                        , inline
                            [ "font-normal", "text-white-60" ]
                            [ text "This'll prevent other people from reading your data." ]
                        ]
                            |> chunk []
                            |> speechBubble

                    UpdateEncryptionKeyScreen _ _ ->
                        [ text "I need a new passphrase to encrypt your personal data."
                        , lineBreak
                        , inline
                            [ "font-normal", "text-white-60" ]
                            [ text "This'll prevent other people from reading your data." ]
                        ]
                            |> chunk []
                            |> speechBubble

                    Welcome ->
                        [ text "Diffuse plays music"
                        , inline [ "not-italic", "font-normal", "mr-px" ] [ text " ♫ " ]
                        , inline [ "font-normal", "text-white-60" ]
                            [ text "from your Dropbox,"
                            , lineBreak
                            , text "IPFS node, Amazon S3 bucket, or any other"
                            , lineBreak
                            , text "cloud/distributed storage service you use."
                            ]
                        ]
                            |> chunk []
                            |> speechBubble

                    _ ->
                        [ text "Where would you like to keep your personal data?"
                        , lineBreak
                        , inline
                            [ "font-normal", "text-white-60" ]
                            [ text "That's things like your favourites, your playlists, etc."
                            , lineBreak
                            , text "After this you'll be able add some music ♫"
                            ]
                        ]
                            |> chunk []
                            |> speechBubble
                ]
            ]

        -----------------------------------------
        -- Content
        -----------------------------------------
        , case state of
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

            Unauthenticated ->
                choicesScreen

            Authenticated _ ->
                choicesScreen

            Authenticating ->
                Html.nothing

            Welcome ->
                welcomeScreen

        -----------------------------------------
        -- Link to about page
        -----------------------------------------
        , chunk
            [ "antialiased"
            , "font-semibold"
            , "flex"
            , "flex-grow"
            , "items-end"
            , "leading-snug"
            , "pb-8"
            , "pt-3"
            , "text-sm"
            ]
            [ slab
                a
                [ href "about/" ]
                [ "border-b"
                , "border-white-60"
                , "italic"
                , "no-underline"
                , "text-white-60"
                ]
                [ text "More info" ]
            ]
        ]



-- WELCOME


welcomeScreen : Html Authentication.Msg
welcomeScreen =
    chunk
        [ "mt-3"
        , "relative"
        , "z-10"
        ]
        [ UI.Kit.buttonWithColor
            UI.Kit.Blank
            UI.Kit.Filled
            GetStarted
            (slab
                Html.span
                [ style "letter-spacing" "0.25em"
                ]
                [ "align-middle"
                , "inline-block"
                , "text-nearly-sm"
                ]
                [ text "SIGN IN" ]
            )
        ]



-- LOADING


negotiating : Html Authentication.Msg
negotiating =
    chunk
        [ "flex"
        , "items-center"
        ]
        [ chunk
            [ "transform", "-translate-y-px" ]
            [ Html.map never (UI.Svg.Elements.loadingWithSize 14) ]
        , chunk
            [ "italic"
            , "ml-2"
            , "text-opacity-80"
            , "text-sm"
            , "text-white"
            ]
            [ Html.text "Negotiating with service" ]
        ]



-- CHOICES


choicesScreen : Html Authentication.Msg
choicesScreen =
    chunk
        [ "bg-white"
        , "rounded"
        , "px-4"
        , "py-2"
        , "relative"
        , "z-10"

        -- Dark mode
        ------------
        , "dark:bg-darkest-hour"
        ]
        [ choiceButton
            { action = ShowNewEncryptionKeyScreen Local
            , icon = Icons.web
            , infoLink = Nothing
            , label = "My Browser"
            , outOfOrder = False
            }
        , choiceButton
            { action = TriggerExternalAuth (Fission { initialised = False }) ""
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.fissionLogo
            , infoLink = Just "https://fission.codes/"
            , label = "Fission"
            , outOfOrder = False
            }
        , choiceButton
            { action = TriggerExternalAuth (Dropbox { accessToken = "", expiresAt = 0, refreshToken = "" }) ""
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.dropboxLogo
            , infoLink = Just "https://dropbox.com/"
            , label = "Dropbox"
            , outOfOrder = False
            }
        , choiceButton
            { action =
                AskForInput
                    (RemoteStorage { userAddress = "", token = "" })
                    { placeholder = "example@5apps.com"
                    , question = """
                        What's your user address?
                        <span class="font-normal text-white-60">
                            <br />The format's
                            <span class="font-semibold">username@server.domain</span>
                        </span>
                      """
                    , value = ""
                    }
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.remoteStorageLogo
            , infoLink = Just "https://remotestorage.io/"
            , label = "RemoteStorage"
            , outOfOrder = False
            }

        -- More options
        ---------------
        , chunk
            [ "pb-px", "pt-4", "text-center" ]
            [ slab
                Html.span
                [ title "More options"
                , Mouse.onClick ShowMoreOptions
                ]
                [ "inline-block", "px-1", "cursor-pointer", "leading-none" ]
                [ chunk
                    [ "pointer-events-none" ]
                    [ Icons.more_horiz 22 Inherit ]
                ]
            ]
        ]


choiceButton :
    { action : msg
    , icon : Int -> Coloring -> Svg msg
    , infoLink : Maybe String
    , label : String
    , outOfOrder : Bool
    }
    -> Html msg
choiceButton { action, icon, infoLink, label, outOfOrder } =
    chunk
        [ "border-b"
        , "border-gray-300"
        , "relative"

        --
        , "last:border-b-0"

        -- Dark mode
        ------------
        , "dark:border-base01"
        ]
        [ -----------------------------------------
          -- Button
          -----------------------------------------
          slab
            button
            [ onClick action ]
            [ "bg-transparent"
            , "cursor-pointer"
            , "flex"
            , "items-center"
            , "leading-none"
            , "min-w-tiny"
            , "outline-none"
            , "px-2"
            , "py-4"
            , "text-left"
            , "text-sm"
            ]
            [ chunk
                [ "flex"
                , "items-center"

                --
                , ifThenElse outOfOrder "opacity-20" "opacity-100"
                ]
                [ inline
                    [ "inline-flex", "mr-4" ]
                    [ icon 16 Inherit ]
                , text label
                ]
            ]

        -----------------------------------------
        -- Info icon
        -----------------------------------------
        , case infoLink of
            Just link ->
                slab
                    Html.a
                    [ style "left" "100%"
                    , style "top" "50%"
                    , style "transform" "translateY(-50%)"

                    --
                    , href link
                    , target "_blank"
                    , title ("Learn more about " ++ label)
                    ]
                    [ "absolute"
                    , "cursor-pointer"
                    , "duration-100"
                    , "leading-none"
                    , "ml-4"
                    , "opacity-40"
                    , "pl-4"
                    , "text-white"
                    , "transition-opacity"
                    , "transform"
                    , "-translate-y-1/2"

                    --
                    , "hocus:opacity-100"
                    ]
                    [ Icons.help 17 Inherit ]

            Nothing ->
                nothing
        ]



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
            , Html.Events.onInput KeepPassphraseInMemory
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
            , Html.Events.onInput Input
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
