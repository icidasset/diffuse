module View exposing (entry)

-- Children

import Color
import Console.View as Console
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy
import Material.Icons.Action
import Material.Icons.Alert
import Navigation.View as Navigation
import Queue.View as Queue
import Routing.Types exposing (Page(..))
import Settings.View as Settings
import Sources.View as Sources
import Spinner.View as Spinner
import Styles exposing (..)
import Svg exposing (Svg, g, path, svg)
import Svg.Attributes exposing (d, fill, fillRule, height, viewBox, width)
import Tracks.View as Tracks
import Types exposing (..)
import Utils exposing (..)


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
                            , text "A music player that connects to your cloud/distributed storage."
                            ]
                        ]
                        model

                -- # Needs authentication
                --
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
        ]



-- Authenticated bits


authenticated : List (Html Msg) -> Model -> Html Msg
authenticated children model =
    if model.authenticatedUser == Nothing then
        unauthenticated
            [ authButton ]
            model
    else
        div
            [ cssClass Shell ]
            [ Html.Lazy.lazy authenticatedNavigation model.routing.currentPage
            , div [ cssClass Insulation ] children
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
            children
        ]


unauthenticatedNavigation : Page -> Html Msg
unauthenticatedNavigation currentPage =
    Navigation.outside
        currentPage
        [ ( Material.Icons.Action.home navColor 16, "/" )
        , ( Material.Icons.Action.info navColor 16, "/about" )
        ]


authButton : Html Msg
authButton =
    a
        [ cssClass AuthenticationButton, onClick Authenticate ]
        [ span
            []
            [ text "Sign in with Blockstack" ]
        ]



-- Shared


navColor : Color.Color
navColor =
    Color.rgba 0 0 0 0.4
