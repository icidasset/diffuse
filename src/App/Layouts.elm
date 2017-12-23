module Layouts exposing (..)

import Element exposing (el, text)
import Element.Attributes exposing (..)
import Element.Types exposing (Node)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onSubmit)
import Utils exposing (cssClass, cssClasses)
import Variables exposing (scaled)


-- Styles

import Styles exposing (Styles(H1, Zed))
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
