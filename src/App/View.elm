module View exposing (entry)

import ContextMenu.Styles as CTS
import Color
import Equalizer.Touch
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, on)
import Html.Lazy
import Json.Decode as Decode
import Material.Icons.Action
import Material.Icons.Alert
import Navigation.View as Navigation
import Routing.Types exposing (Page(..))
import Styles exposing (..)
import Svg exposing (Svg, g, path, svg)
import Svg.Attributes exposing (d, fill, fillRule, height, viewBox, width)
import Traits exposing (intoRem)
import Types exposing (..)
import Utils exposing (..)
import Variables exposing (colors, colorDerivatives)


-- Children

import Console.View as Console
import Equalizer.View as Equalizer
import Queue.View as Queue
import Settings.View as Settings
import Sources.View as Sources
import Spinner.View as Spinner
import Tracks.View as Tracks


-- Children, Pt. 2

import Queue.Types
import Sources.Types


-- 🍯


entry : Model -> Html Msg
entry model =
    div
        (-- ++ Equalizer Events
         -- TODO: Find a way to make these into subscriptions
         --       (like with the mouse events)
         case model.equalizer.activeKnob of
            Just _ ->
                (if model.isTouchDevice then
                    [ on "tap" (Decode.succeed ClickAway)
                    , on "touchmove" Equalizer.Touch.move
                    , on "touchend" Equalizer.Touch.end
                    ]
                 else
                    [ onClick ClickAway ]
                )

            Nothing ->
                [ if model.isTouchDevice then
                    on "tap" (Decode.succeed ClickAway)
                  else
                    onClick ClickAway
                ]
        )
        [ --
          -- {override} Loading
          --
          if model.showLoadingScreen then
            div
                [ cssClass InTheMiddle ]
                [ Spinner.entry ]
            --
            -- Default
            --
          else
            case model.routing.currentPage of
                -- # Doesn't need authentication
                --
                ErrorScreen err ->
                    div
                        [ cssClass Shell ]
                        [ if model.authenticatedUser == Nothing then
                            unauthenticatedNavigation model.routing.currentPage
                          else
                            authenticatedNavigation model.routing.currentPage
                        , div
                            [ cssClasses [ InTheMiddle, Basic ] ]
                            [ p
                                []
                                [ Material.Icons.Alert.error Color.white 14
                                , strong [] [ text err ]
                                ]
                            ]
                        ]

                -- # Needs authentication
                --
                Equalizer ->
                    authenticated
                        [ Equalizer.entry model ]
                        model

                Index ->
                    authenticated
                        [ Tracks.entry model ]
                        model

                Queue queuePage ->
                    authenticated
                        [ Queue.entry queuePage model ]
                        model

                Settings ->
                    authenticated
                        [ Settings.entry model ]
                        model

                Sources sourcePage ->
                    authenticated
                        [ Sources.entry sourcePage model ]
                        model

        --
        -- Context menu
        --
        , Html.Lazy.lazy contextMenu model.contextMenu

        --
        -- Overlay
        --
        , div
            [ cssClass Overlay
            , case model.contextMenu of
                Just _ ->
                    style [ ( "opacity", "1" ) ]

                Nothing ->
                    style []
            ]
            []
        ]



-- Authenticated bits


authenticated : List (Html Msg) -> Model -> Html Msg
authenticated children model =
    if model.authenticatedUser == Nothing then
        unauthenticated
            [ div
                [ style
                    [ ( "background-color", "white" )
                    , ( "border-radius", "4px" )
                    , ( "padding", ".375rem 1.5rem" )
                    ]
                ]
                [ authButton Blockstack
                , authButton Local
                ]
            ]
            model
    else
        div
            [ cssClass Shell ]
            [ -- Navigation
              --
              Html.Lazy.lazy
                authenticatedNavigation
                model.routing.currentPage

            -- Page
            --
            , div [ cssClass Insulation ] children

            -- Console
            --
            , Console.entry model
            ]


authenticatedNavigation : Page -> Html Msg
authenticatedNavigation currentPage =
    Navigation.outside
        currentPage
        [ ( text "Tracks", Routing.Types.Index )
        , ( text "Queue", Routing.Types.Queue Queue.Types.Index )
        , ( text "Sources", Routing.Types.Sources Sources.Types.Index )
        , ( text "Settings", Routing.Types.Settings )
        ]



-- Unauthenticated bits


unauthenticated : List (Html Msg) -> Model -> Html Msg
unauthenticated children model =
    div
        [ cssClass Shell ]
        [ unauthenticatedNavigation model.routing.currentPage
        , div
            [ cssClasses [ InTheMiddle, Basic ] ]
            children
        ]


unauthenticatedNavigation : Page -> Html Msg
unauthenticatedNavigation currentPage =
    Navigation.outsideOutgoing
        currentPage
        [ ( Material.Icons.Action.home colors.base05 16, "/" )
        , ( Material.Icons.Action.info colors.base05 16, "/about" )
        ]


authButton : AuthMethod -> Html Msg
authButton authMethod =
    a
        [ cssClass AuthenticationButton, onClick (Authenticate authMethod) ]
        (case authMethod of
            Local ->
                [ span
                    [ style [ ( "fontSize", intoRem 17 ) ] ]
                    [ Material.Icons.Action.lock Color.white 17 ]
                , text "Sign in anonymously"
                ]

            Blockstack ->
                [ span
                    [ style [ ( "fontSize", intoRem 22 ) ] ]
                    [ blockstackLogo ]
                , text "Sign in with Blockstack"
                ]
        )


blockstackLogo : Svg Msg
blockstackLogo =
    Svg.svg
        [ height "22"
        , viewBox "0 0 512 512"
        , width "22"
        ]
        [ g
            [ fill "none", fillRule "evenodd" ]
            [ path
                [ d "M186.673352,210.979293 C163.559368,210.979293 144.821782,192.302202 144.821782,169.262843 C144.821782,146.223483 163.559368,127.546392 186.673352,127.546392 C209.787336,127.546392 228.524922,146.223483 228.524922,169.262843 C228.524922,192.302202 209.787336,210.979293 186.673352,210.979293 Z M354.079631,210.979293 C330.965647,210.979293 312.228061,192.302202 312.228061,169.262843 C312.228061,146.223483 330.965647,127.546392 354.079631,127.546392 C377.193615,127.546392 395.931201,146.223483 395.931201,169.262843 C395.931201,192.302202 377.193615,210.979293 354.079631,210.979293 Z M354.079631,377.845096 C330.965647,377.845096 312.228061,359.168005 312.228061,336.128646 C312.228061,313.089286 330.965647,294.412195 354.079631,294.412195 C377.193615,294.412195 395.931201,313.089286 395.931201,336.128646 C395.931201,359.168005 377.193615,377.845096 354.079631,377.845096 Z M186.673352,377.845096 C163.559368,377.845096 144.821782,359.168005 144.821782,336.128646 C144.821782,313.089286 163.559368,294.412195 186.673352,294.412195 C209.787336,294.412195 228.524922,313.089286 228.524922,336.128646 C228.524922,359.168005 209.787336,377.845096 186.673352,377.845096 Z"
                , fill "#2C96FF"
                ]
                []
            , path
                [ d "M158.85157,222.072077 C135.737586,222.072077 117,203.394986 117,180.355626 C117,157.316266 135.737586,138.639175 158.85157,138.639175 C181.965554,138.639175 200.70314,157.316266 200.70314,180.355626 C200.70314,203.394986 181.965554,222.072077 158.85157,222.072077 Z M326.257849,222.072077 C303.143865,222.072077 284.406279,203.394986 284.406279,180.355626 C284.406279,157.316266 303.143865,138.639175 326.257849,138.639175 C349.371833,138.639175 368.109419,157.316266 368.109419,180.355626 C368.109419,203.394986 349.371833,222.072077 326.257849,222.072077 Z M326.257849,388.93788 C303.143865,388.93788 284.406279,370.260789 284.406279,347.221429 C284.406279,324.18207 303.143865,305.504978 326.257849,305.504978 C349.371833,305.504978 368.109419,324.18207 368.109419,347.221429 C368.109419,370.260789 349.371833,388.93788 326.257849,388.93788 Z M158.85157,388.93788 C135.737586,388.93788 117,370.260789 117,347.221429 C117,324.18207 135.737586,305.504978 158.85157,305.504978 C181.965554,305.504978 200.70314,324.18207 200.70314,347.221429 C200.70314,370.260789 181.965554,388.93788 158.85157,388.93788 Z"
                , fill "#E91E63"
                ]
                []
            , path
                [ d "M172.762461,205.432902 C149.648477,205.432902 130.910891,186.75581 130.910891,163.716451 C130.910891,140.677091 149.648477,122 172.762461,122 C195.876445,122 214.614031,140.677091 214.614031,163.716451 C214.614031,186.75581 195.876445,205.432902 172.762461,205.432902 Z M340.16874,205.432902 C317.054756,205.432902 298.31717,186.75581 298.31717,163.716451 C298.31717,140.677091 317.054756,122 340.16874,122 C363.282724,122 382.02031,140.677091 382.02031,163.716451 C382.02031,186.75581 363.282724,205.432902 340.16874,205.432902 Z M340.16874,372.298705 C317.054756,372.298705 298.31717,353.621614 298.31717,330.582254 C298.31717,307.542894 317.054756,288.865803 340.16874,288.865803 C363.282724,288.865803 382.02031,307.542894 382.02031,330.582254 C382.02031,353.621614 363.282724,372.298705 340.16874,372.298705 Z M172.762461,372.298705 C149.648477,372.298705 130.910891,353.621614 130.910891,330.582254 C130.910891,307.542894 149.648477,288.865803 172.762461,288.865803 C195.876445,288.865803 214.614031,307.542894 214.614031,330.582254 C214.614031,353.621614 195.876445,372.298705 172.762461,372.298705 Z"
                , fill "#270F34"
                ]
                []
            ]
        ]



-- Shared


contextMenu : Maybe ContextMenu -> Html Msg
contextMenu maybeContextMenu =
    case maybeContextMenu of
        Just (ContextMenu items mousePos) ->
            div
                [ cssClass CTS.ContextMenu
                , style
                    [ ( "left", toString mousePos.x ++ "px" )
                    , ( "top", toString mousePos.y ++ "px" )
                    ]
                ]
                (List.map
                    (\( icon, label, msg ) -> a [ onClick msg ] [ icon, text label ])
                    items
                )

        _ ->
            text ""
