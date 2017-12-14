module Abroad.View exposing (..)

import Abroad.Ports
import Abroad.Types exposing (Msg(..))
import Authentication.UserData
import Color.Convert exposing (colorToCssRgb)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onSubmit)
import Json.Decode
import Material.Icons.Navigation as Icons
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Routing.Types
import String.Interpolate exposing (interpolate)
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (..)
import Variables exposing (colorDerivatives)


-- Styles

import StylesOld exposing (Classes(..))


-- 🍯


entry : Model -> Html TopLevel.Msg
entry model =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.inside
            [ ( Icon Icons.arrow_back
              , Label (Hidden "Go back")
              , Routing.Types.Settings
              )
            ]

        ------------------------------------
        -- Content
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "Import / Export" ]
            , p
                [ cssClass Intro ]
                [ text """
                    All your data will be replaced when you import something.
                  """
                ]
            , Html.form
                [ onSubmit (AbroadMsg Import)
                ]
                [ importView model
                , exportView model
                ]
            ]
        ]



-- Import


importView : Model -> Html TopLevel.Msg
importView model =
    p
        []
        [ label
            []
            [ text "Import" ]

        -- Input
        --
        , input
            [ accept ".json"
            , id Abroad.Ports.importFileInputId
            , name Abroad.Ports.importFileInputId
            , on "change" (Json.Decode.succeed <| AbroadMsg FileSelectedForImport)
            , type_ "file"
            ]
            []

        -- Label
        --
        , label
            [ for Abroad.Ports.importFileInputId
            , cssClasses [ Button, ButtonSubtle ]
            ]
            [ text "Choose file" ]
        , if model.abroad.fileSelected then
            button
                [ cssClasses [ Button ]
                , type_ "submit"
                ]
                [ text "Import" ]
          else
            text ""

        -- Message
        --
        , em
            [ style
                [ ( "display", "block" )
                , ( "font-size", "0.8em" )
                , ( "margin-top", ".375rem" )
                ]
            ]
            [ case model.abroad.importMessage of
                Ok "" ->
                    if model.abroad.fileSelected then
                        span
                            []
                            [ text "Click on the "
                            , strong [] [ text "import" ]
                            , text " button to confirm."
                            ]
                    else
                        text ""

                Ok msg ->
                    span
                        []
                        [ text msg ]

                Err msg ->
                    span
                        [ style
                            [ ( "color", colorToCssRgb colorDerivatives.error ) ]
                        ]
                        [ text msg ]
            ]
        ]



-- Export


exportView : Model -> Html TopLevel.Msg
exportView model =
    p
        []
        [ label
            []
            [ text "Export" ]
        , let
            js =
                interpolate
                    """
                    var a = this;
                    var json = JSON.stringify({0}, null, 4);
                    var file = new Blob([json], { type: "application/json" });
                    a.href = window.URL.createObjectURL(file);
                    a.download = "isotach.json";
                    """
                    [ Authentication.UserData.outwards model ]
          in
            a
                [ cssClasses [ Button ]
                , attribute "onclick" js
                ]
                [ text "Export" ]
        ]
