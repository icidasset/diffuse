module UI.Authentication exposing (Model(..), Msg(..), extractMethod, initialModel, update, view)

import Alien
import Base64
import Binary
import Chunky exposing (..)
import Color
import Color.Ext as Color
import Common exposing (Switch(..))
import Conditional exposing (..)
import Css exposing (pct, px, solid, transparent)
import Css.Classes as C
import Css.Global
import Html exposing (Html, a, button, img, span, text)
import Html.Attributes exposing (attribute, href, placeholder, src, style, target, title, value, width)
import Html.Events exposing (onClick, onSubmit)
import Html.Events.Extra exposing (onClickStopPropagation)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Encode
import Markdown
import Material.Icons exposing (Coloring(..))
import Material.Icons.Action as Icons
import Material.Icons.Alert as Icons
import Material.Icons.Av as Icons
import Material.Icons.Content as Icons
import Material.Icons.Navigation as Icons
import Maybe.Extra as Maybe
import Return3 exposing (..)
import SHA
import String.Ext as String
import Svg exposing (Svg)
import Tachyons.Classes as T
import UI.Kit exposing (ButtonType(..))
import UI.Ports as Ports
import UI.Reply exposing (Reply(..))
import UI.Svg.Elements
import Url exposing (Url)
import Url.Ext as Url
import User.Layer exposing (..)



-- ⛩


minimumPassphraseLength =
    16


passphraseLengthErrorMessage =
    "Your passphrase should be atleast *16 characters* long."



-- 🌳


type Model
    = Authenticated Method
    | InputScreen Method Question
    | NewEncryptionKeyScreen Method (Maybe String)
    | UpdateEncryptionKeyScreen Method (Maybe String)
    | Unauthenticated
    | Welcome


type alias Question =
    { placeholder : String
    , question : String
    , value : String
    }


initialModel : Url -> Model
initialModel url =
    case Url.action url of
        [ "authenticate", "dropbox" ] ->
            url.fragment
                |> Maybe.map (String.split "&")
                |> Maybe.map (List.filter <| String.startsWith "access_token=")
                |> Maybe.andThen List.head
                |> Maybe.withDefault ""
                |> String.replace "access_token=" ""
                |> (\t ->
                        NewEncryptionKeyScreen
                            (Dropbox { token = t })
                            Nothing
                   )

        [ "authenticate", "remotestorage", encodedUserAddress ] ->
            let
                userAddress =
                    encodedUserAddress
                        |> Url.percentDecode
                        |> Maybe.andThen (Base64.decode >> Result.toMaybe)
                        |> Maybe.withDefault encodedUserAddress
            in
            url.fragment
                |> Maybe.map (String.split "&")
                |> Maybe.map (List.filter <| String.startsWith "access_token=")
                |> Maybe.andThen List.head
                |> Maybe.withDefault ""
                |> String.replace "access_token=" ""
                |> (\t ->
                        NewEncryptionKeyScreen
                            (RemoteStorage { userAddress = userAddress, token = t })
                            Nothing
                   )

        _ ->
            Welcome


extractMethod : Model -> Maybe Method
extractMethod model =
    case model of
        Authenticated method ->
            Just method

        InputScreen method _ ->
            Just method

        NewEncryptionKeyScreen method _ ->
            Just method

        UpdateEncryptionKeyScreen method _ ->
            Just method

        Unauthenticated ->
            Nothing

        Welcome ->
            Nothing



-- 📣


type Msg
    = Bypass
    | Cancel
    | GetStarted
    | ShowMoreOptions Mouse.Event
    | SignIn Method
    | SignInWithPassphrase Method String
    | SignedIn Method
    | TriggerExternalAuth Method String
      -----------------------------------------
      -- Encryption
      -----------------------------------------
    | KeepPassphraseInMemory String
    | RemoveEncryptionKey Method
    | ShowNewEncryptionKeyScreen Method
    | ShowUpdateEncryptionKeyScreen Method
    | UpdateEncryptionKey Method String
      -----------------------------------------
      -- IPFS
      -----------------------------------------
    | PingIpfs
    | PingIpfsCallback (Result Http.Error ())
    | PingOtherIpfs String
    | PingOtherIpfsCallback String (Result Http.Error ())
      -----------------------------------------
      -- More Input
      -----------------------------------------
    | AskForInput Method Question
    | Input String
    | ConfirmInput
      -----------------------------------------
      -- Textile
      -----------------------------------------
    | PingTextile
    | PingTextileCallback (Result Http.Error ())
    | PingOtherTextile String
    | PingOtherTextileCallback String (Result Http.Error ())


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            return model

        Cancel ->
            ( case model of
                Authenticated method ->
                    Authenticated method

                InputScreen _ _ ->
                    Unauthenticated

                NewEncryptionKeyScreen _ _ ->
                    Unauthenticated

                UpdateEncryptionKeyScreen method _ ->
                    Authenticated method

                Unauthenticated ->
                    Welcome

                Welcome ->
                    Welcome
              --
            , Cmd.none
            , [ ForceTracksRerender ]
            )

        GetStarted ->
            return Unauthenticated

        ShowMoreOptions mouseEvent ->
            ( model
            , Cmd.none
            , ( mouseEvent.clientPos
              , mouseEvent.offsetPos
              )
                |> (\( ( a, b ), ( c, d ) ) ->
                        { x = a - c + 15
                        , y = b - d + 12
                        }
                   )
                |> ShowMoreAuthenticationOptions
                |> List.singleton
            )

        SignIn method ->
            ( model
              --
            , [ ( "method", encodeMethod method )
              , ( "passphrase", Json.Encode.null )
              ]
                |> Json.Encode.object
                |> Alien.broadcast Alien.SignIn
                |> Ports.toBrain
              --
            , [ ToggleLoadingScreen On ]
            )

        SignInWithPassphrase method passphrase ->
            if String.length passphrase < minimumPassphraseLength then
                addReply
                    (ShowErrorNotification passphraseLengthErrorMessage)
                    (return model)

            else
                ( model
                  --
                , [ ( "method", encodeMethod method )
                  , ( "passphrase", Json.Encode.string <| hashPassphrase passphrase )
                  ]
                    |> Json.Encode.object
                    |> Alien.broadcast Alien.SignIn
                    |> Ports.toBrain
                  --
                , [ ToggleLoadingScreen On ]
                )

        SignedIn method ->
            return (Authenticated method)

        TriggerExternalAuth method string ->
            returnReplyWithModel model (ExternalAuth method string)

        -----------------------------------------
        -- Encryption
        -----------------------------------------
        KeepPassphraseInMemory passphrase ->
            case model of
                NewEncryptionKeyScreen method _ ->
                    return (NewEncryptionKeyScreen method <| Just passphrase)

                UpdateEncryptionKeyScreen method _ ->
                    return (UpdateEncryptionKeyScreen method <| Just passphrase)

                _ ->
                    return model

        RemoveEncryptionKey method ->
            Alien.RemoveEncryptionKey
                |> Alien.trigger
                |> Ports.toBrain
                |> returnCommandWithModel (Authenticated method)
                |> addReply ForceTracksRerender
                |> addReply (ShowSuccessNotification "Saving data without encryption ...")

        ShowNewEncryptionKeyScreen method ->
            return (NewEncryptionKeyScreen method Nothing)

        ShowUpdateEncryptionKeyScreen method ->
            return (UpdateEncryptionKeyScreen method Nothing)

        UpdateEncryptionKey method passphrase ->
            if String.length passphrase < minimumPassphraseLength then
                addReply
                    (ShowErrorNotification passphraseLengthErrorMessage)
                    (return model)

            else
                passphrase
                    |> hashPassphrase
                    |> Json.Encode.string
                    |> Alien.broadcast Alien.UpdateEncryptionKey
                    |> Ports.toBrain
                    |> returnCommandWithModel (Authenticated method)
                    |> addReply ForceTracksRerender
                    |> addReply (ShowSuccessNotification "Encrypting data with new passphrase ...")

        -----------------------------------------
        -- IPFS
        -----------------------------------------
        PingIpfs ->
            { url = "//localhost:5001/api/v0/id"
            , expect = Http.expectWhatever PingIpfsCallback
            }
                |> Http.get
                |> returnCommandWithModel model

        PingIpfsCallback (Ok _) ->
            { apiOrigin = "//localhost:5001" }
                |> Ipfs
                |> ShowNewEncryptionKeyScreen
                |> updateWithModel model

        PingIpfsCallback (Err _) ->
            { placeholder = "//localhost:5001"
            , question = """
                Where's your IPFS API located?<br />
                <span class="fw4 white-60">
                    You can find this address on the IPFS Web UI.<br />
                    Most likely you'll also need to setup CORS.<br />
                    You can find the instructions for that
                    <a href="about#CORS__IPFS" target="_blank" class="bb color-inherit fw6 link">here</a>.
                </span>
              """
            , value = "//localhost:5001"
            }
                |> AskForInput (Ipfs { apiOrigin = "" })
                |> updateWithModel model

        PingOtherIpfs origin ->
            { url = origin ++ "/api/v0/id"
            , expect = Http.expectWhatever (PingOtherIpfsCallback origin)
            }
                |> Http.get
                |> returnCommandWithModel model

        PingOtherIpfsCallback origin (Ok _) ->
            { apiOrigin = origin }
                |> Ipfs
                |> ShowNewEncryptionKeyScreen
                |> updateWithModel model

        PingOtherIpfsCallback origin (Err _) ->
            "Can't reach this IPFS API, maybe it's offline? Or I don't have access?"
                |> ShowErrorNotification
                |> returnReplyWithModel model

        -----------------------------------------
        -- More Input
        -----------------------------------------
        AskForInput method opts ->
            { placeholder = opts.placeholder
            , question = opts.question
            , value = opts.value
            }
                |> InputScreen method
                |> return

        Input string ->
            case model of
                InputScreen method opts ->
                    return (InputScreen method { opts | value = string })

                m ->
                    return m

        ConfirmInput ->
            case model of
                InputScreen (Ipfs i) { value } ->
                    value
                        |> String.chopEnd "/"
                        |> PingOtherIpfs
                        |> updateWithModel model

                InputScreen (RemoteStorage r) { value } ->
                    addReply
                        (ExternalAuth (RemoteStorage r) value)
                        (return model)

                InputScreen (Textile t) { value } ->
                    value
                        |> String.chopEnd "/"
                        |> PingOtherTextile
                        |> updateWithModel model

                _ ->
                    return model

        -----------------------------------------
        -- Textile
        -----------------------------------------
        PingTextile ->
            { url = "//localhost:40600/api/v0/summary"
            , expect = Http.expectWhatever PingTextileCallback
            }
                |> Http.get
                |> returnCommandWithModel model

        PingTextileCallback (Ok _) ->
            { apiOrigin = "//localhost:40600" }
                |> Textile
                |> SignIn
                |> updateWithModel model

        PingTextileCallback (Err _) ->
            { placeholder = "//localhost:40600"
            , question = """
                Where's your Textile API located?<br />
                <span class="fw4 white-60">
                    You might need to do some CORS configuration.<br />
                    You can find the instructions for that
                    <a href="about#CORS__Textile" target="_blank" class="bb color-inherit fw6 link">here</a>.<br />
                    You can't connect to a HTTP server while on HTTPS.
                </span>
              """
            , value = "//localhost:40600"
            }
                |> AskForInput (Textile { apiOrigin = "" })
                |> updateWithModel model

        PingOtherTextile origin ->
            { url = origin ++ "/api/v0/summary"
            , expect = Http.expectWhatever (PingOtherTextileCallback origin)
            }
                |> Http.get
                |> returnCommandWithModel model

        PingOtherTextileCallback origin (Ok _) ->
            { apiOrigin = origin }
                |> Textile
                |> SignIn
                |> updateWithModel model

        PingOtherTextileCallback origin (Err _) ->
            "Can't reach this Textile API, maybe it's offline? Or I don't have access?"
                |> ShowErrorNotification
                |> returnReplyWithModel model


updateWithModel : Model -> Msg -> Return Model Msg Reply
updateWithModel model msg =
    update msg model


hashPassphrase : String -> String
hashPassphrase phrase =
    phrase
        |> Binary.fromStringAsUtf8
        |> SHA.sha256
        |> Binary.toHex
        |> String.toLower



-- 🗺


view : Model -> Html Msg
view model =
    chunk
        [ C.flex
        , C.flex_col
        , C.h_full
        , C.items_center
        ]
        [ brick
            [ style "height" "42%" ]
            [ C.flex
            , C.items_center
            , C.p_4

            --
            , C.md__pb_0
            ]
            [ chunk
                [ C.py_4, C.relative ]
                [ img
                    [ onClick Cancel
                    , src "images/diffuse-light.svg"
                    , width 190

                    --
                    , case model of
                        Welcome ->
                            title "Diffuse"

                        _ ->
                            title "Go back"

                    --
                    , case model of
                        Welcome ->
                            style "cursor" "default"

                        _ ->
                            style "cursor" "pointer"
                    ]
                    []

                -- Speech bubble
                ----------------
                , case model of
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
                            [ C.font_normal, C.text_white_60 ]
                            [ text "This'll prevent other people from reading your data." ]
                        ]
                            |> chunk []
                            |> speechBubble

                    UpdateEncryptionKeyScreen _ _ ->
                        [ text "I need a new passphrase to encrypt your personal data."
                        , lineBreak
                        , inline
                            [ C.font_normal, C.text_white_60 ]
                            [ text "This'll prevent other people from reading your data." ]
                        ]
                            |> chunk []
                            |> speechBubble

                    Welcome ->
                        [ text "Diffuse plays music"
                        , inline [ C.not_italic, C.font_normal, C.mr_px ] [ text " ♫ " ]
                        , inline [ C.font_normal, C.text_white_60 ]
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
                            [ C.font_normal, C.text_white_60 ]
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
        , case model of
            InputScreen method opts ->
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

            Welcome ->
                welcomeScreen

        -----------------------------------------
        -- Link to about page
        -----------------------------------------
        , chunk
            [ T.f6
            , T.fw6
            , T.flex
            , T.flex_grow_1
            , T.items_end
            , T.lh_title
            , T.pb4
            , T.pt3
            ]
            [ slab
                a
                [ href "about" ]
                [ T.bb
                , T.i
                , T.no_underline
                , T.white_60
                ]
                [ text "More info" ]
            ]
        ]


inputSpeechStyles : List Css.Style
inputSpeechStyles =
    [ Css.Global.descendants
        [ Css.Global.p
            [ Css.margin Css.zero ]
        ]
    ]



-- WELCOME


welcomeScreen : Html Msg
welcomeScreen =
    chunk
        [ T.mt3
        , T.relative
        , T.z_1
        ]
        [ UI.Kit.buttonWithColor
            (Color.rgb 1 1 1)
            Filled
            GetStarted
            (slab
                Html.span
                [ style "color" "#A19B9D"
                , style "font-size" "13px"
                , style "line-height" "20px"
                ]
                [ T.tracked_mega ]
                [ text "SIGN IN" ]
            )
        ]



-- CHOICES


choicesScreen : Html Msg
choicesScreen =
    chunk
        [ T.bg_white
        , T.br2
        , T.ph3
        , T.pv2
        , T.relative
        , T.z_1
        ]
        [ choiceButton
            { action = ShowNewEncryptionKeyScreen Local
            , icon = Icons.web
            , infoLink = Nothing
            , isLast = False
            , label = "My Browser"
            , outOfOrder = False
            }
        , choiceButton
            { action = TriggerExternalAuth Blockstack ""
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.blockstackLogo
            , infoLink = Just "https://blockstack.org"
            , isLast = False
            , label = "Blockstack"
            , outOfOrder = False
            }
        , choiceButton
            { action = TriggerExternalAuth (Dropbox { token = "" }) ""
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.dropboxLogo
            , infoLink = Just "https://dropbox.com/"
            , isLast = False
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
                        <span class="fw4 white-60">
                            <br />The format's
                            <span class="fw6">username@server.domain</span>
                        </span>
                      """
                    , value = ""
                    }
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.remoteStorageLogo
            , infoLink = Just "https://remotestorage.io/"
            , isLast = False
            , label = "RemoteStorage"
            , outOfOrder = False
            }

        -- More options
        ---------------
        , chunk
            [ T.pb1, T.pt3, T.tc ]
            [ slab
                Html.span
                [ title "More options"
                , Mouse.onClick ShowMoreOptions
                ]
                [ T.dib, T.ph1, T.pointer, C.leading_none ]
                [ chunk
                    [ C.pointer_events_none ]
                    [ Icons.more_horiz 22 Inherit ]
                ]
            ]
        ]


choiceButton :
    { action : msg
    , icon : Int -> Coloring -> Svg msg
    , infoLink : Maybe String
    , isLast : Bool
    , label : String
    , outOfOrder : Bool
    }
    -> Html msg
choiceButton { action, icon, infoLink, isLast, label, outOfOrder } =
    chunk
        [ T.relative ]
        [ -----------------------------------------
          -- Button
          -----------------------------------------
          slab
            button
            [ onClick action ]
            [ C.cursor_pointer
            , C.flex
            , C.items_center
            , C.leading_none
            , C.outline_none
            , C.py_4
            , C.text_left
            , C.text_sm
            ]
            [ chunk
                [ C.flex
                , C.items_center

                --
                , ifThenElse outOfOrder C.opacity_20 C.opacity_100
                ]
                [ slab
                    span
                    []
                    [ C.inline_flex, C.mr_4 ]
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
                    [ T.absolute, T.glow, C.leading_none, T.ml3, T.o_40, T.pl3, T.pointer, T.white ]
                    [ Icons.help 17 Inherit ]

            Nothing ->
                nothing
        ]



-- ENCRYPTION KEY


encryptionKeyScreen : { withEncryption : Msg, withoutEncryption : Msg } -> Html Msg
encryptionKeyScreen { withEncryption, withoutEncryption } =
    slab
        Html.form
        [ onSubmit withEncryption ]
        [ T.flex
        , T.flex_column
        ]
        [ UI.Kit.textArea
            [ attribute "autocapitalize" "none"
            , attribute "autocomplete" "off"
            , attribute "autocorrect" "off"
            , attribute "spellcheck" "false"

            --
            , placeholder "anQLS9Usw24gxUi11IgVBg76z8SCWZgLKkoWIeJ1ClVmBHLRlaiA0CtvONVAMGritbgd3U45cPTxrhFU0WXaOAa8pVt186KyEccfUNyAq97"

            --
            , Html.Events.onInput KeepPassphraseInMemory
            ]
        , UI.Kit.button
            Filled
            Bypass
            (text "Continue")
        , brick
            [ onClickStopPropagation withoutEncryption ]
            [ T.f7
            , T.flex
            , T.items_center
            , T.justify_center
            , T.lh_title
            , T.mt3
            , T.pointer
            , T.white_50
            ]
            [ inline [ T.dib, C.leading_none, T.mr2 ] [ Icons.warning 13 Inherit ]
            , text "Continue without encryption"
            ]
        ]



-- INPUT SCREEN


inputScreen : Question -> Html Msg
inputScreen question =
    slab
        Html.form
        [ onSubmit ConfirmInput ]
        [ T.flex
        , T.flex_column
        ]
        [ UI.Kit.textFieldAlt
            [ attribute "autocapitalize" "off"
            , placeholder question.placeholder
            , Html.Events.onInput Input
            , value question.value
            ]
        , UI.Kit.button
            UI.Kit.Filled
            Bypass
            (text "Continue")
        ]



-- SPEECH BUBBLE


speechBubble : Html msg -> Html msg
speechBubble contents =
    chunk
        [ C.absolute
        , C.antialiased
        , C.bg_background
        , C.border_b
        , C.border_transparent
        , C.font_semibold
        , C.italic
        , C.leading_snug
        , C.left_half_way
        , C.neg_translate_x_half_way
        , C.px_4
        , C.py_2
        , C.rounded
        , C.text_center
        , C.text_sm
        , C.text_white
        , C.top_full
        , C.translate_put_on_top
        , C.whitespace_no_wrap
        ]
        [ contents

        --
        , brick
            speechBubbleArrowStyles
            [ C.absolute
            , C.h_0
            , C.left_half_way
            , C.top_0
            , C.w_0
            ]
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


speechBubbleArrowStyles : List (Html.Attribute Msg)
speechBubbleArrowStyles =
    let
        color =
            Color.toCssString UI.Kit.colors.background
    in
    [ style "border-color" ("transparent transparent " ++ color ++ " transparent")
    , style "border-width" "0 6px 5px 6px"
    ]
