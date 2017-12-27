module Layouts exposing (..)

import Color
import Color.Convert
import Element exposing (el, empty, html, row, text)
import Element.Attributes exposing (..)
import Element.Types exposing (Attr, Node)
import Html exposing (Html, div, option)
import Html.Attributes exposing (selected, style, value)
import Html.Events exposing (onSubmit)
import Material.Icons.Navigation as Icons
import Types as TopLevel
import Utils exposing (cssClass, cssClasses)
import Variables exposing (colorDerivatives, scaled)


-- Styles

import Form.Styles
import Styles exposing (Styles(Form, H1, Zed))
import StylesOld exposing (Classes(..))


-- 🍯 / Old


centeredForm : msg -> Html msg -> Html msg
centeredForm submitMsg childNode =
    Html.form
        [ cssClasses
            [ InsulationContent
            , InsulationFlexContent
            , InsulationCentered
            ]
        , style
            [ ( "position", "relative" )
            , ( "text-align", "center" )
            ]
        , onSubmit submitMsg
        ]
        [ div
            [ cssClasses
                [ InsulationFlexContent ]
            , style
                [ ( "overflow", "hidden" )
                , ( "position", "relative" )
                , ( "width", "100%" )
                , ( "z-index", "9" )
                ]
            ]
            [ div
                [ cssClasses
                    [ InsulationContent
                    , InsulationCentered
                    ]
                ]
                [ div
                    [ cssClasses [ ContentBox ]
                    , style [ ( "padding-top", "2.25rem" ) ]
                    ]
                    [ childNode ]
                ]
            ]
        , div
            [ cssClass LogoBackdrop ]
            []
        ]



-- 🍯


btn : Styles -> List Attr -> Node -> Node
btn buttonStyles additionalAttributes buttonChild =
    el
        buttonStyles
        (List.append
            [ paddingXY (scaled -5) (scaled -10) ]
            (additionalAttributes)
        )
        buttonChild


h1 : String -> Node
h1 label =
    Element.h1
        H1
        [ inlineStyle [ ( "display", "table" ) ]
        , moveUp 1
        , paddingBottom (scaled -14)
        , paddingLeft (scaled -4)
        , paddingRight (scaled -4)
        , paddingTop (scaled -10)
        ]
        (text label)


intro : List Node -> Node
intro children =
    Element.paragraph
        Styles.Intro
        [ paddingBottom (scaled 7), paddingTop (scaled 4) ]
        children


lbl : String -> Node
lbl theLabel =
    el (Form Form.Styles.Label) [] (text theLabel)


logoBackdrop : Node
logoBackdrop =
    el
        Styles.LogoBackdrop
        [ height fill, width fill ]
        empty


select : List ( String, String ) -> String -> (String -> TopLevel.Msg) -> Node
select options selectedValue onInputMsg =
    Element.within
        [ 20
            |> Icons.expand_more (Color.greyscale 0.325)
            |> html
            |> el Zed
                [ alignRight
                , inlineStyle [ ( "line-height", "0" ), ( "pointer-events", "none" ) ]
                , verticalCenter
                ]
        ]
        (Html.select
            [ Html.Events.onInput onInputMsg
            , Html.Attributes.style
                [ ( "-moz-appearance", "none" )
                , ( "-webkit-appearance", "none" )
                , ( "appearance", "none" )
                , ( "background", "transparent" )
                , ( "border", "0" )
                , ( "color", Color.Convert.colorToCssRgb colorDerivatives.text )
                , ( "font-family", "Overpass, sans-serif" )
                , ( "font-size", (toString <| scaled 0) ++ "px" )
                , ( "padding", (toString <| scaled -3) ++ "px 0" )
                , ( "width", "100%" )
                ]
            ]
            (List.map
                (\( val, lbl ) ->
                    option
                        [ selected (val == selectedValue), value val ]
                        [ Html.text lbl ]
                )
                options
            )
            |> html
            |> el (Form Form.Styles.Input) []
        )
