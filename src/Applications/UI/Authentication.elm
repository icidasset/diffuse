module UI.Authentication exposing (Model(..), Msg(..), extractMethod, initialModel, update, view)

import Alien
import Authentication exposing (Method(..))
import Base64
import Binary
import Chunky exposing (..)
import Classes as C
import Color exposing (Color)
import Color.Ext as Color
import Common exposing (Switch(..))
import Conditional exposing (..)
import Css exposing (pct, px, solid, transparent)
import Html.Events.Extra.Mouse as Mouse
import Html.Styled as Html exposing (Html, a, button, em, fromUnstyled, img, span, text)
import Html.Styled.Attributes as Attributes exposing (attribute, css, href, placeholder, src, style, title, value, width)
import Html.Styled.Events exposing (onClick, onSubmit)
import Http
import Json.Encode
import Material.Icons exposing (Coloring(..))
import Material.Icons.Av as Icons
import Material.Icons.Navigation as Icons
import Return3 as Return exposing (..)
import SHA
import String.Ext as String
import Svg exposing (Svg)
import Tachyons.Classes as T
import UI.Kit exposing (ButtonType(..))
import UI.Ports as Ports
import UI.Reply exposing (Reply(..))
import UI.Svg.Elements
import Url exposing (Url)



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
    case String.split "/" (String.dropLeft 1 url.path) of
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
      -----------------------------------------
      -- Encryption
      -----------------------------------------
    | KeepPassphraseInMemory String
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
            ( Unauthenticated
            , Cmd.none
            , []
            )

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
            ( Unauthenticated
              --
            , [ ( "method", Authentication.encodeMethod method )
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
                ( Unauthenticated
                  --
                , [ ( "method", Authentication.encodeMethod method )
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
                ( Authenticated method
                , passphrase
                    |> hashPassphrase
                    |> Json.Encode.string
                    |> Alien.broadcast Alien.UpdateEncryptionKey
                    |> Ports.toBrain
                , [ ForceTracksRerender ]
                )

        -----------------------------------------
        -- IPFS
        -----------------------------------------
        PingIpfs ->
            { url = "http://localhost:5001/api/v0/id"
            , expect = Http.expectWhatever PingIpfsCallback
            }
                |> Http.get
                |> returnCommandWithModel model

        PingIpfsCallback (Ok _) ->
            { apiOrigin = "http://localhost:5001" }
                |> Ipfs
                |> ShowNewEncryptionKeyScreen
                |> updateWithModel model

        PingIpfsCallback (Err _) ->
            { placeholder = "http://localhost:5001"
            , question = "Where's your IPFS API located?"
            , value = "http://localhost:5001"
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
            "Can't reach this IPFS API, maybe it's offline?"
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
            { url = "http://localhost:40600/api/v0/summary"
            , expect = Http.expectWhatever PingTextileCallback
            }
                |> Http.get
                |> returnCommandWithModel model

        PingTextileCallback (Ok _) ->
            { apiOrigin = "http://localhost:40600" }
                |> Textile
                |> SignIn
                |> updateWithModel model

        PingTextileCallback (Err _) ->
            { placeholder = "http://localhost:40600"
            , question = "Where's your Textile API located?"
            , value = "http://localhost:40600"
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
            "Can't reach this Textile API, maybe it's offline?"
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
                [ T.pv3, T.relative ]
                [ img
                    [ onClick Cancel
                    , src "/images/diffuse-light.svg"
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
                            |> text
                            |> speechBubble

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

                    Welcome ->
                        [ inline [ T.i ] [ text "Hi, I'm D1ff-B0t, I play music" ]
                        , lineBreak
                        , inline [ T.i ] [ text "♫ from your digital storage." ]
                        ]
                            |> chunk []
                            |> speechBubble

                    _ ->
                        [ text "Where would you like to"
                        , lineBreak
                        , text "store your encrypted data?"
                        , lineBreak
                        , inline [ T.i, T.o_50 ] [ text "favourites, settings, etc." ]
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
            , T.lh_solid
            , T.pb4
            , T.pt3
            ]
            [ slab
                a
                [ href "/about" ]
                [ T.no_underline
                , T.white_60
                ]
                [ em [] [ text "What is this exactly?" ] ]
            ]
        ]



-- WELCOME


welcomeScreen : Html Msg
welcomeScreen =
    chunk
        [ T.relative
        , T.z_1
        ]
        [ UI.Kit.buttonWithColor
            UI.Kit.colors.background
            Normal
            GetStarted
            (slab
                Html.span
                [ style "font-size" "13px"
                , style "line-height" "20px"
                ]
                [ T.tracked_mega ]
                [ text "GET STARTED" ]
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
            { action =
                AskForInput
                    (RemoteStorage { userAddress = "", token = "" })
                    { placeholder = "username@remote.storage"
                    , question = "What's your user address?"
                    , value = ""
                    }
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.remoteStorageLogo
            , isLast = False
            , label = "RemoteStorage"
            , outOfOrder = False
            }
        , choiceButton
            { action =
                PingTextile
            , icon = \_ _ -> Svg.map never UI.Svg.Elements.textileLogo
            , isLast = False
            , label = "Textile"
            , outOfOrder = False
            }

        -- More options
        ---------------
        , chunk
            [ T.pb1, T.pt3, T.tc ]
            [ slab
                Html.span
                [ title "More options"
                , Attributes.fromUnstyled (Mouse.onClick ShowMoreOptions)
                ]
                [ T.dib, T.ph1, T.pointer, C.lh_0 ]
                [ chunk
                    [ C.pointer_events_none ]
                    [ fromUnstyled (Icons.more_horiz 22 Inherit) ]
                ]
            ]
        ]


choiceButton :
    { action : msg
    , icon : Int -> Coloring -> Svg msg
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
                [ fromUnstyled (icon 16 Inherit) ]
            , text label
            ]
        ]



-- ENCRYPTION KEY


encryptionKeyScreen : Msg -> Html Msg
encryptionKeyScreen msg =
    slab
        Html.form
        [ onSubmit msg ]
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
            Filled
            Bypass
            (text "Continue")
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
            [ placeholder question.placeholder
            , Html.Styled.Events.onInput Input
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
    brick
        [ css speechBubbleStyles ]
        [ T.absolute
        , T.br2
        , T.f6
        , T.lh_copy
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
