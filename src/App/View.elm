module View exposing (entry)

import Alfred.View
import Authentication.Types exposing (Method(..))
import ContextMenu.Styles as CTS
import Color
import Equalizer.Touch
import Html exposing (Html, br, em, div, p, strong)
import Html.Attributes exposing (style)
import Html.Events
import Html.Lazy
import Json.Decode as Decode
import Material.Icons.Action
import Material.Icons.Alert
import Maybe.Extra as Maybe
import Navigation.View as Navigation
import Notifications.Config
import Notifications.Types exposing (Notification)
import Notifications.View
import Routing.Types exposing (Page(..))
import StylesOld
import Svg exposing (Svg)
import Svg.Attributes
import Toasty
import Traits exposing (intoRem)
import Types exposing (..)
import Utils exposing (..)
import Variables exposing (colors, colorDerivatives, insulationWidth, scaled)


-- Styles

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Element.Ext as Element
import Styles exposing (..)


-- Children

import Abroad.View as Abroad
import Console.View as Console
import Equalizer.View as Equalizer
import Playlists.View as Playlists
import Queue.View as Queue
import Settings.View as Settings
import Sources.View as Sources
import Spinner.View as Spinner
import Tracks.View as Tracks


-- Children, Pt. 2

import Sources.Types


-- ⚗️


type alias Node =
    Element Styles Variations Msg


type alias Attr =
    Element.Attribute Variations Msg


type alias InsulationOptions =
    { nested : Bool }



-- 🍯


entry : Model -> Html Msg
entry model =
    [ entryNodes model
    , entryLazyNodes model
    ]
        |> List.concat
        |> column Root (entryAttributes model)
        |> viewport Styles.styles


entryNodes : Model -> List Node
entryNodes model =
    if model.showLoadingScreen then
        ------------------------------------
        -- {1} Loading
        ------------------------------------
        [ loadingScreen
        ]
    else if requiresAuthentication model then
        ------------------------------------
        -- {2} Authentication is required
        ------------------------------------
        [ mainNav model
        , loginScreen model
        ]
    else
        ------------------------------------
        -- {3} Otherwise
        ------------------------------------
        [ mainNav model

        -- Main
        --
        , if model.authentication.signedIn then
            model
                |> Tracks.entry
                |> html
                |> insulate unnested
                |> within [ currentPage nested model ]
          else
            currentPage unnested model

        -- Console
        --
        , if model.authentication.signedIn then
            html (Console.entry model)
          else
            empty
        ]


entryLazyNodes : Model -> List Node
entryLazyNodes model =
    [ Element.lazy alfred model.alfred
    , Element.lazy backgroundImage model.settings.backgroundImage
    , Element.lazy contextMenu model.contextMenu
    , Element.lazy notifications model.toasties
    , Element.lazy2 overlay model.alfred model.contextMenu
    ]


{-| Global mouse/touch events & Equalizer events

TODO: Find a way to make these into subscriptions
(like with the mouse events)

-}
entryAttributes : Model -> List Attr
entryAttributes model =
    -- TODO
    -- case model.equalizer.activeKnob of
    --     Just _ ->
    --         (if model.isTouchDevice then
    --             [ on "tap" (Decode.succeed ClickAway)
    --             , on "touchmove" Equalizer.Touch.move
    --             , on "touchend" Equalizer.Touch.end
    --             ]
    --          else
    --             [ onClick ClickAway ]
    --         )
    --
    --     Nothing ->
    --         [ if model.isTouchDevice then
    --             on "tap" (Decode.succeed ClickAway)
    --           else
    --             onClick ClickAway
    --         ]
    [ height fill
    , onClick ClickAway
    , paddingXY (scaled 2) 0
    ]



-- 🐝


requiresAuthentication : Model -> Bool
requiresAuthentication model =
    case model.routing.currentPage of
        ErrorScreen _ ->
            False

        MessageScreen _ ->
            False

        _ ->
            not model.authentication.signedIn


{-| Render current page
-}
currentPage : InsulationOptions -> Model -> Node
currentPage insulationOptions model =
    case model.routing.currentPage of
        -- # Doesn't require authentication
        --
        ErrorScreen err ->
            -- TODO
            -- div
            --     [{- cssClasses [ InTheMiddle, Basic ] -}]
            --     [ p
            --         []
            --         [ Material.Icons.Alert.error Color.white 14
            --         , strong [] [ Html.text err ]
            --         ]
            --     ]
            empty

        MessageScreen message ->
            -- let
            --     parts =
            --         String.split "\n" message
            -- in
            --     div
            --         [{- cssClasses [ InTheMiddle, Basic ] -}]
            --         [ p
            --             []
            --             (List.map
            --                 (\part -> em [] [ Html.text part, br [] [] ])
            --                 parts
            --             )
            --         ]
            empty

        -- # Requires authentication
        --
        Abroad ->
            model
                |> Abroad.entry
                |> html
                |> insulate insulationOptions

        Equalizer ->
            model
                |> Equalizer.entry
                |> html
                |> insulate insulationOptions

        Index ->
            empty

        Playlists playlistsPage ->
            model
                |> Playlists.entry playlistsPage
                |> html
                |> insulate insulationOptions

        Queue queuePage ->
            model
                |> Queue.entry queuePage
                |> html
                |> insulate insulationOptions

        Settings ->
            model
                |> Settings.entry
                |> html
                |> insulate insulationOptions

        Sources sourcePage ->
            model
                |> Sources.entry sourcePage
                |> html
                |> insulate insulationOptions



-- Screens


loadingScreen : Node
loadingScreen =
    Spinner.entry
        |> html
        |> el Zed [ center, verticalCenter ]
        |> el Zed [ height fill ]


loginScreen : Model -> Node
loginScreen model =
    el
        Zed
        [ height fill ]
        (el
            Zed
            [ center, verticalCenter ]
            (column
                AuthenticationOptions
                [ paddingXY (scaled 5) (scaled -8) ]
                [ authButton model.isHTTPS Blockstack
                , authButton model.isHTTPS RemoteStorage
                , authButton model.isHTTPS Local
                ]
            )
        )



-- Authenticated bits


authenticatedNavigation : Page -> Maybe Alfred -> Node
authenticatedNavigation currentPage maybeAlfred =
    let
        styles =
            case maybeAlfred of
                Just _ ->
                    [ ( "visibility", "hidden" ) ]

                Nothing ->
                    []
    in
        el
            Zed
            [ inlineStyle styles ]
            (Navigation.outside
                currentPage
                [ ( Html.text "Tracks", Routing.Types.Index )
                , ( Html.text "Sources", Routing.Types.Sources Sources.Types.Index )
                , ( Html.text "Settings", Routing.Types.Settings )
                ]
                |> html
            )



-- Unauthenticated bits


unauthenticatedNavigation : Page -> Node
unauthenticatedNavigation currentPage =
    Navigation.outsideOutgoing
        currentPage
        [ ( Material.Icons.Action.home colors.base05 16, "/" )
        , ( Material.Icons.Action.info colors.base05 16, "/about" )
        ]
        |> html


authButton : Bool -> Authentication.Types.Method -> Node
authButton isHTTPS authMethod =
    el
        AuthenticationButton
        [ minWidth (px 260)
        , paddingXY 0 (scaled -1)
        , spacing 1

        -- Events
        , authMethod
            |> Authentication.Types.PerformSignIn
            |> AuthenticationMsg
            |> onClick
        ]
        (row
            Zed
            [ spacing (scaled -2) ]
            (case authMethod of
                Local ->
                    authButtonInterior
                        ( Material.Icons.Action.lock colorDerivatives.text 17
                        , "Sign in anonymously"
                        )

                Blockstack ->
                    authButtonInterior
                        ( blockstackLogo
                        , "Sign in with Blockstack"
                        )

                RemoteStorage ->
                    authButtonInterior
                        ( remoteStorageLogo
                        , "Sign in with Remote Storage"
                        )
            )
        )


authButtonInterior : ( Html Msg, String ) -> List Node
authButtonInterior ( icon, label ) =
    [ el WithoutLineHeight
        [ moveUp 1, verticalCenter, width (px 22) ]
        (el Zed [ center ] <| html icon)

    --
    , text label
    ]


blockstackLogo : Svg msg
blockstackLogo =
    Svg.svg
        [ Svg.Attributes.height "22"
        , Svg.Attributes.viewBox "0 0 512 512"
        , Svg.Attributes.width "22"
        ]
        [ Svg.g
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.fillRule "evenodd"
            ]
            [ Svg.path
                [ Svg.Attributes.d "M186.673352,210.979293 C163.559368,210.979293 144.821782,192.302202 144.821782,169.262843 C144.821782,146.223483 163.559368,127.546392 186.673352,127.546392 C209.787336,127.546392 228.524922,146.223483 228.524922,169.262843 C228.524922,192.302202 209.787336,210.979293 186.673352,210.979293 Z M354.079631,210.979293 C330.965647,210.979293 312.228061,192.302202 312.228061,169.262843 C312.228061,146.223483 330.965647,127.546392 354.079631,127.546392 C377.193615,127.546392 395.931201,146.223483 395.931201,169.262843 C395.931201,192.302202 377.193615,210.979293 354.079631,210.979293 Z M354.079631,377.845096 C330.965647,377.845096 312.228061,359.168005 312.228061,336.128646 C312.228061,313.089286 330.965647,294.412195 354.079631,294.412195 C377.193615,294.412195 395.931201,313.089286 395.931201,336.128646 C395.931201,359.168005 377.193615,377.845096 354.079631,377.845096 Z M186.673352,377.845096 C163.559368,377.845096 144.821782,359.168005 144.821782,336.128646 C144.821782,313.089286 163.559368,294.412195 186.673352,294.412195 C209.787336,294.412195 228.524922,313.089286 228.524922,336.128646 C228.524922,359.168005 209.787336,377.845096 186.673352,377.845096 Z"
                , Svg.Attributes.fill "#2C96FF"
                ]
                []
            , Svg.path
                [ Svg.Attributes.d "M158.85157,222.072077 C135.737586,222.072077 117,203.394986 117,180.355626 C117,157.316266 135.737586,138.639175 158.85157,138.639175 C181.965554,138.639175 200.70314,157.316266 200.70314,180.355626 C200.70314,203.394986 181.965554,222.072077 158.85157,222.072077 Z M326.257849,222.072077 C303.143865,222.072077 284.406279,203.394986 284.406279,180.355626 C284.406279,157.316266 303.143865,138.639175 326.257849,138.639175 C349.371833,138.639175 368.109419,157.316266 368.109419,180.355626 C368.109419,203.394986 349.371833,222.072077 326.257849,222.072077 Z M326.257849,388.93788 C303.143865,388.93788 284.406279,370.260789 284.406279,347.221429 C284.406279,324.18207 303.143865,305.504978 326.257849,305.504978 C349.371833,305.504978 368.109419,324.18207 368.109419,347.221429 C368.109419,370.260789 349.371833,388.93788 326.257849,388.93788 Z M158.85157,388.93788 C135.737586,388.93788 117,370.260789 117,347.221429 C117,324.18207 135.737586,305.504978 158.85157,305.504978 C181.965554,305.504978 200.70314,324.18207 200.70314,347.221429 C200.70314,370.260789 181.965554,388.93788 158.85157,388.93788 Z"
                , Svg.Attributes.fill "#E91E63"
                ]
                []
            , Svg.path
                [ Svg.Attributes.d "M172.762461,205.432902 C149.648477,205.432902 130.910891,186.75581 130.910891,163.716451 C130.910891,140.677091 149.648477,122 172.762461,122 C195.876445,122 214.614031,140.677091 214.614031,163.716451 C214.614031,186.75581 195.876445,205.432902 172.762461,205.432902 Z M340.16874,205.432902 C317.054756,205.432902 298.31717,186.75581 298.31717,163.716451 C298.31717,140.677091 317.054756,122 340.16874,122 C363.282724,122 382.02031,140.677091 382.02031,163.716451 C382.02031,186.75581 363.282724,205.432902 340.16874,205.432902 Z M340.16874,372.298705 C317.054756,372.298705 298.31717,353.621614 298.31717,330.582254 C298.31717,307.542894 317.054756,288.865803 340.16874,288.865803 C363.282724,288.865803 382.02031,307.542894 382.02031,330.582254 C382.02031,353.621614 363.282724,372.298705 340.16874,372.298705 Z M172.762461,372.298705 C149.648477,372.298705 130.910891,353.621614 130.910891,330.582254 C130.910891,307.542894 149.648477,288.865803 172.762461,288.865803 C195.876445,288.865803 214.614031,307.542894 214.614031,330.582254 C214.614031,353.621614 195.876445,372.298705 172.762461,372.298705 Z"
                , Svg.Attributes.fill "#270F34"
                ]
                []
            ]
        ]


remoteStorageLogo : Svg msg
remoteStorageLogo =
    Svg.svg
        [ Svg.Attributes.height "17"
        , Svg.Attributes.viewBox "0 0 739 853"
        , Svg.Attributes.width "17"

        --
        , Html.Attributes.style
            [ ( "shape-rendering", "geometricPrecision" )
            , ( "text-rendering", "geometricPrecision" )
            , ( "image-rendering", "optimizeQuality" )
            , ( "clip-rule", "evenodd" )
            , ( "fill-rule", "evenodd" )
            ]
        ]
        [ Svg.polygon
            [ Svg.Attributes.fill "#FF4B03"
            , Svg.Attributes.points "370,754 0,542 0,640 185,747 370,853 554,747 739,640 739,525 739,525 739,476 739,427 739,378 653,427 370,589 86,427 86,427 86,361 185,418 370,524 554,418 653,361 739,311 739,213 739,213 554,107 370,0 185,107 58,180 144,230 228,181 370,100 511,181 652,263 370,425 87,263 87,263 0,213 0,213 0,311 0,378 0,427 0,476 86,525 185,582 370,689 554,582 653,525 653,590 653,592"
            ]
            []
        ]



-- Insulation


insulate : InsulationOptions -> Node -> Node
insulate insulationOptions node =
    el
        (if insulationOptions.nested then
            NestedInsulation
         else
            Insulation
        )
        [ clip
        , center
        , height fill
        , maxWidth (px insulationWidth)
        , width fill
        ]
        node


nested : InsulationOptions
nested =
    { nested = True }


unnested : InsulationOptions
unnested =
    { nested = False }



-- Shared


alfred : Maybe Alfred -> Node
alfred maybe =
    case maybe of
        Just context ->
            html <| Alfred.View.entry context

        Nothing ->
            empty


backgroundImage : String -> Node
backgroundImage img =
    el
        Zed
        [ inlineStyle (backgroundImageStyles img)
        , height fill
        , width fill
        ]
        empty


backgroundImageStyles : String -> List ( String, String )
backgroundImageStyles img =
    [ ( "background-image", "url(/images/Background/" ++ img ++ ")" )
    , ( "background-size", "cover" )
    , ( "left", "0" )
    , ( "position", "fixed" )
    , ( "top", "0" )
    , ( "z-index", "-10" )

    --
    , case img of
        "1.jpg" ->
            ( "background-position", "center 30%" )

        _ ->
            ( "background-position", "center bottom" )
    ]


contextMenu : Maybe ContextMenu -> Node
contextMenu maybeContextMenu =
    -- TODO
    -- case maybeContextMenu of
    --     Just (ContextMenu items mousePos) ->
    --         div
    --             [ cssClass CTS.ContextMenu
    --             , onWithOptions "click" contextMenuEventOptions (Decode.succeed NoOp)
    --             , onWithOptions "tap" contextMenuEventOptions (Decode.succeed NoOp)
    --             , style
    --                 [ ( "left", toString mousePos.x ++ "px" )
    --                 , ( "top", toString mousePos.y ++ "px" )
    --                 ]
    --             ]
    --             (List.map
    --                 (\( icon, label, msg ) ->
    --                     a
    --                         [ onClick (DoAll [ msg, HideContextMenu ]) ]
    --                         [ icon, text label ]
    --                 )
    --                 items
    --             )
    --
    --     _ ->
    --         empty
    empty


contextMenuEventOptions : Html.Events.Options
contextMenuEventOptions =
    { preventDefault = True
    , stopPropagation = True
    }


mainNav : Model -> Node
mainNav model =
    el
        Zed
        [ center
        , paddingBottom (scaled 12)
        , paddingTop (scaled 10)
        , width fill
        ]
        (if model.authentication.signedIn then
            Element.lazy2 authenticatedNavigation model.routing.currentPage model.alfred
         else
            Element.lazy unauthenticatedNavigation model.routing.currentPage
        )


notifications : Toasty.Stack Notification -> Node
notifications stack =
    html
        (Toasty.view
            Notifications.Config.config
            Notifications.View.entry
            ToastyMsg
            stack
        )


overlay : Maybe Alfred -> Maybe ContextMenu -> Node
overlay maybeAlfred maybeContextMenu =
    let
        additionalStyles =
            if Maybe.isJust maybeAlfred || Maybe.isJust maybeContextMenu then
                [ ( "cursor", "pointer" )
                , ( "opacity", "1" )
                ]
            else
                [ ( "pointer-events", "none" )
                ]
    in
        el
            Overlay
            [ overlayPositioning
                |> List.append additionalStyles
                |> inlineStyle
            ]
            empty


overlayPositioning : List ( String, String )
overlayPositioning =
    [ ( "left", "0" )
    , ( "position", "fixed" )
    , ( "top", "0" )
    , ( "z-index", "900" )
    ]
