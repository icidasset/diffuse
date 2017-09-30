module Abroad.View exposing (..)

import Abroad.Types exposing (Msg(..))
import Authentication.UserData
import Color.Convert exposing (colorToCssRgb)
import FileReader exposing (onFileChange)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit)
import Material.Icons.Navigation as Icons
import Navigation.View as Navigation
import Routing.Types
import String.Interpolate exposing (interpolate)
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (..)
import Variables exposing (colorDerivatives)


-- Styles

import Styles exposing (Classes(..))


-- 🍯


entry : Model -> Html TopLevel.Msg
entry model =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.inside
            [ ( span
                    []
                    [ Icons.arrow_back colorDerivatives.text 16
                    , label [] [ text "Settings" ]
                    ]
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
                [ onSubmit (AbroadMsg UploadFiles) ]
                [ --
                  -- Import
                  --
                  p
                    []
                    [ label
                        []
                        [ text "Import" ]
                    , input
                        [ type_ "file"
                        , name "fileInput"
                        , id "fileInput"
                        , accept ".json"
                        , onFileChange (SetFiles >> AbroadMsg)
                        ]
                        []
                    , label
                        [ for "fileInput"
                        , cssClasses [ Button, ButtonSmall, ButtonSubtle ]
                        ]
                        [ text "Choose file" ]
                    , if List.isEmpty model.abroad.files then
                        text ""
                      else
                        button
                            [ cssClasses [ Button, ButtonSmall ]
                            , type_ "submit"
                            ]
                            [ text "Import" ]
                    , em
                        [ style
                            [ ( "display", "block" )
                            , ( "font-size", "0.8em" )
                            , ( "margin-top", ".375rem" )
                            ]
                        ]
                        [ case model.abroad.importMessage of
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

                --
                -- Export
                --
                , p
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
                                a.download = "ongaku-ryoho.json";
                                """
                                [ Authentication.UserData.outwards model ]
                      in
                        a
                            [ cssClasses [ Button, ButtonSmall ]
                            , attribute "onclick" js
                            ]
                            [ text "Export" ]
                    ]
                ]
            ]
        ]
