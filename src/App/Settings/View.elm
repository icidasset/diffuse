module Settings.View exposing (..)

import Authentication.Types as Authentication exposing (Method(..))
import Color
import Html exposing (Html, div, label, option, select)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onInput)
import Material.Icons.Action as Icons
import Material.Icons.Communication as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Image as Icons
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Routing.Types exposing (Page(..))
import Settings.Types
import Types exposing (Model, Msg(..), Node)
import Utils exposing (cssClass)


-- Elements

import Element exposing (..)
import Element.Attributes exposing (..)


-- Styles

import Form.StylesOld as FormStyles
import StylesOld exposing (Classes(..))
import Styles exposing (Styles(..))


-- 🍯


entry : Model -> Node
entry model =
    column
        Zed
        []
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideCustomNew
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
        , column
            Zed
            []
            [ h1
                Zed
                []
                (text "Settings")
            , paragraph
                Zed
                []
                [ text "Changes are automatically saved."
                , html (Html.br [] [])
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
            , model
                |> theForm
                |> Html.map SettingsMsg
                |> html
            ]
        ]


theForm : Model -> Html Settings.Types.Msg
theForm model =
    Html.form
        [ cssClass FormStyles.HalfWidthForm ]
        [ label
            []
            [ Html.text "Background image" ]
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
                            [ Html.text lbl ]
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
