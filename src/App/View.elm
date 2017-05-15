module View exposing (entry)

import Color
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy
import Material.Icons.Action
import Material.Icons.Alert
import Navigation.View as Navigation
import Routing.Types exposing (Page(..))
import Styles exposing (..)
import Svg exposing (Svg, g, path, svg)
import Svg.Attributes exposing (d, fill, fillRule, height, viewBox, width)
import Types exposing (..)
import Utils exposing (..)


-- Children

import Console.View as Console
import Queue.View as Queue
import Settings.View as Settings
import Sources.View as Sources
import Spinner.View as Spinner
import Tracks.View as Tracks


-- 🍯


entry : Model -> Html Msg
entry model =
    div
        []
        [ -- {override} Loading
          if model.showLoadingScreen then
            div
                [ cssClass InTheMiddle ]
                [ Spinner.entry ]
          else
            case model.routing.currentPage of
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

                About ->
                    unauthenticated
                        [ p
                            []
                            [ strong [] [ text "Ongaku Ryoho." ]
                            , br [] []
                            , text "A music player that connects to your cloud storage."
                            ]
                        ]
                        (model)

                Custom ->
                    unauthenticated
                        [ p
                            []
                            [ strong [] [ text "Coming soon." ]
                            , br [] []
                            , text """
                                Will allow you to use
                                a personal Firebase app.
                              """
                            ]
                        ]
                        (model)

                -- # Needs authentication
                --
                Index ->
                    authenticated
                        [ Tracks.entry model ]
                        (model)

                Queue queuePage ->
                    authenticated
                        [ Queue.entry queuePage model ]
                        (model)

                Settings ->
                    authenticated
                        [ Settings.entry model ]
                        (model)

                Sources sourcePage ->
                    authenticated
                        [ Sources.entry sourcePage model ]
                        (model)
        ]



-- Authenticated bits


authenticated : List (Html Msg) -> Model -> Html Msg
authenticated children model =
    if model.authenticatedUser == Nothing then
        unauthenticated
            [ authButton ]
            (model)
    else
        div
            [ cssClass Shell ]
            [ Html.Lazy.lazy authenticatedNavigation model.routing.currentPage
            , div [ cssClass Insulation ] (children)
            , Console.entry model
            ]


authenticatedNavigation : Page -> Html Msg
authenticatedNavigation currentPage =
    Navigation.outside
        currentPage
        [ ( text "Tracks", "/" )
        , ( text "Queue", "/queue" )
        , ( text "Sources", "/sources" )
        , ( text "Settings", "/settings" )
        ]



-- Unauthenticated bits


unauthenticated : List (Html Msg) -> Model -> Html Msg
unauthenticated children model =
    div
        [ cssClass Shell ]
        [ unauthenticatedNavigation model.routing.currentPage
        , div
            [ cssClasses [ InTheMiddle, Basic ] ]
            (children)
        ]


unauthenticatedNavigation : Page -> Html Msg
unauthenticatedNavigation currentPage =
    Navigation.outside
        currentPage
        [ ( Material.Icons.Action.home navColor 16, "/" )
        , ( Material.Icons.Action.info navColor 16, "/about" )
        , ( Material.Icons.Action.settings navColor 16, "/customize" )
        ]


authButton : Html Msg
authButton =
    a
        [ cssClass AuthenticationButton, onClick Authenticate ]
        [ googleLogo
        , span
            []
            [ text "Sign in with Google" ]
        ]


googleLogo : Svg Msg
googleLogo =
    Svg.svg
        [ height "512"
        , cssSvgClass AuthenticationButtonLogo
        , viewBox "0 0 512 512"
        , width "512"
        ]
        [ g
            [ fill "none", fillRule "evenodd" ]
            [ path
                [ d "M482.56 261.36c0-16.73-1.5-32.83-4.29-48.27H256v91.29h127.01c-5.47 29.5-22.1 54.49-47.09 71.23v59.21h76.27c44.63-41.09 70.37-101.59 70.37-173.46z"
                , fill "#4285f4"
                ]
                []
            , path
                [ d "M256 492c63.72 0 117.14-21.13 156.19-57.18l-76.27-59.21c-21.13 14.16-48.17 22.53-79.92 22.53-61.47 0-113.49-41.51-132.05-97.3H45.1v61.15c38.83 77.13 118.64 130.01 210.9 130.01z"
                , fill "#34a853"
                ]
                []
            , path
                [ d "M123.95 300.84c-4.72-14.16-7.4-29.29-7.4-44.84s2.68-30.68 7.4-44.84V150.01H45.1C29.12 181.87 20 217.92 20 256c0 38.08 9.12 74.13 25.1 105.99l78.85-61.15z"
                , fill "#fbbc05"
                ]
                []
            , path
                [ d "M256 113.86c34.65 0 65.76 11.91 90.22 35.29l67.69-67.69C373.03 43.39 319.61 20 256 20c-92.25 0-172.07 52.89-210.9 130.01l78.85 61.15c18.56-55.78 70.59-97.3 132.05-97.3z"
                , fill "#ea4335"
                ]
                []
            , path
                [ d "M20 20h472v472H20V20z" ]
                []
            ]
        ]



-- Shared


navColor : Color.Color
navColor =
    Color.rgba 0 0 0 0.4
