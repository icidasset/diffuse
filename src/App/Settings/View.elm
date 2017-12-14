module Settings.View exposing (..)

import Authentication.Types as Authentication exposing (Method(..))
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Material.Icons.Action as Icons
import Material.Icons.Communication as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Image as Icons
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Routing.Types exposing (Page(..))
import Settings.Types
import Types exposing (Model, Msg(..))
import Utils exposing (cssClass)


-- Styles

import Form.Styles as FormStyles
import StylesOld exposing (Classes(..))


-- 🍯


entry : Model -> Html Msg
entry model =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideCustom
            [ ( Icon Icons.import_export
              , Label (Shown "Import / Export")
                --
              , RoutingMsg (Routing.Types.GoToPage Abroad)
              )
            , ( Icon Icons.exit_to_app
              , Label (Shown "Sign out")
                --
              , AuthenticationMsg Authentication.PerformSignOut
              )
            ]

        ------------------------------------
        -- Content
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "Settings" ]
            , p
                [ cssClass Intro ]
                [ text "Changes are automatically saved."
                , br [] []
                , text "PS. You are using the "
                , case Maybe.withDefault Local model.authentication.method of
                    Blockstack ->
                        text "Blockstack"

                    Local ->
                        text "anonymous"

                    RemoteStorage ->
                        text "RemoteStorage"
                , text " authentication mode."
                ]
            , Html.map SettingsMsg (theForm model)
            ]
        ]


theForm : Model -> Html Settings.Types.Msg
theForm model =
    Html.form
        [ cssClass FormStyles.HalfWidthForm ]
        [ label
            []
            [ text "Background image" ]
        , div
            [ cssClass FormStyles.SelectBox ]
            [ select
                [ onInput Settings.Types.SetBackgroundImage ]
                (List.map
                    (\( val, lbl ) ->
                        option
                            [ selected (val == model.settings.backgroundImage)
                            , value val
                            ]
                            [ text lbl ]
                    )
                    backgroundImages
                )
            , Icons.expand_more (Color.greyscale 0.325) 20
            ]
        ]


backgroundImages : List ( String, String )
backgroundImages =
    [ ( "1.jpg", "Option 1" )
    , ( "2.jpg", "Option 2" )
    , ( "3.jpg", "Option 3" )
    , ( "4.jpg", "Option 4" )
    , ( "5.jpg", "Option 5" )
    , ( "6.jpg", "Option 6" )
    , ( "7.jpg", "Option 7 (default)" )
    ]
