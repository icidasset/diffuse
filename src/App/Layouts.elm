module Layouts exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onSubmit)
import Utils exposing (cssClass, cssClasses)


-- Styles

import StylesOld exposing (Classes(..))


-- 🍯


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
