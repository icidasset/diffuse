module Abroad.View exposing (..)

import Abroad.Ports
import Abroad.Types exposing (Msg(..))
import Authentication.UserData
import Color.Convert exposing (colorToCssRgb)
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Material.Icons.Navigation as Icons
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Routing.Types
import String.Interpolate exposing (interpolate)
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (..)
import Variables exposing (colorDerivatives, scaled)


-- Elements

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
import Element.Types exposing (Node)
import Layouts exposing (btn, lbl)


-- Styles

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
          Navigation.insideNew
            [ ( Icon Icons.arrow_back
              , Label (Hidden "Go back")
              , Routing.Types.Settings
              )
            ]

        ------------------------------------
        -- Content
        ------------------------------------
        , column
            Zed
            [ paddingXY (scaled 4) 0 ]
            [ Layouts.h1 "Import / Export"
            , Layouts.intro
                [ text """
                    All your data will be replaced when you import something.
                  """
                ]
            , column
                Zed
                [ spacingXY 0 (scaled 1) ]
                [ importView model
                , exportView model
                ]
            ]
        ]



-- Import


importView : Model -> Node
importView model =
    column
        Zed
        [ spacing (scaled -10) ]
        [ lbl "Import"

        --
        , row
            Zed
            []
            [ -- Input
              --
              Html.input
                [ Html.Attributes.accept ".json"
                , Html.Attributes.id Abroad.Ports.importFileInputId
                , Html.Attributes.name Abroad.Ports.importFileInputId
                , Html.Attributes.type_ "file"

                --
                , Html.Events.on "change" (Json.Decode.succeed <| AbroadMsg FileSelectedForImport)

                --
                , Html.Attributes.style
                    [ ( "height", "0.1px" )
                    , ( "opacity", "0" )
                    , ( "overflow", "hidden" )
                    , ( "position", "absolute" )
                    , ( "width", "0.1px" )
                    , ( "z-index", "-1" )
                    ]
                ]
                []
                |> html

            -- Button 1
            --
            , let
                margin =
                    toString (scaled -8) ++ "px"
              in
                Html.label
                    [ Html.Attributes.for Abroad.Ports.importFileInputId
                    , Html.Attributes.style [ ( "margin-right", margin ) ]
                    ]
                    [ text "Choose file"
                        |> btn SubtleButton []
                        |> toHtml Styles.styles
                    ]
                    |> html

            -- Button 2
            --
            , if model.abroad.fileSelected then
                btn Button [ onClick (AbroadMsg Import) ] (text "Import")
              else
                empty
            ]

        --
        , el
            InlineMessage
            [ paddingTop (scaled -10) ]
            (case model.abroad.importMessage of
                Ok "" ->
                    if model.abroad.fileSelected then
                        paragraph
                            Zed
                            []
                            [ text "Click on the "
                            , bold "import"
                            , text " button to confirm."
                            ]
                    else
                        empty

                Ok msg ->
                    text msg

                Err msg ->
                    el Faulty [] (text msg)
            )
        ]



-- Export


exportView : Model -> Node
exportView model =
    column
        Zed
        [ spacing (scaled -10) ]
        [ lbl "Export"
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
            Html.a
                [ Html.Attributes.attribute "onclick" js ]
                [ Html.text "Export" ]
                |> html
                |> btn Button []
                |> el Zed [ inlineStyle [ ( "display", "inline-block" ) ] ]
                |> el Zed []
        ]
