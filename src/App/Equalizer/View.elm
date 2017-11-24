module Equalizer.View exposing (entry)

import Equalizer.State exposing (maxAngle)
import Equalizer.Touch
import Equalizer.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Json.Decode as Decode
import Material.Icons.Navigation as Icons
import Mouse
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Routing.Types
import Svg exposing (Svg, svg, polygon)
import Svg.Attributes exposing (height, points, viewBox, width)
import Traits exposing (gr)
import Types as TopLevel
import Utils exposing (..)
import Variables exposing (colorDerivatives)


-- Styles

import Equalizer.Styles exposing (Classes(..))
import Styles exposing (Classes(..))


-- 🍯


entry : TopLevel.Model -> Html TopLevel.Msg
entry model =
    div
        [ cssClasses
            [ InsulationContent
            , InsulationFlexContent
            ]
        ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.inside
            [ ( Icon Icons.arrow_back
              , Label (Hidden "Go back")
              , Routing.Types.Index
              )
            ]

        ------------------------------------
        -- Content
        ------------------------------------
        , div
            [ cssClass EqualizerContainer ]
            [ h1
                []
                [ text "EQ" ]
            , div
                [ cssClass LogoBackdrop ]
                []
            , div
                [ cssClass Equalizer ]
                [ div
                    [ cssClass KnobColumn ]
                    [ knob Volume model.equalizer.volume
                    , knobLabel "Volume"
                    ]
                , div
                    [ cssClass KnobColumn ]
                    [ knob Low model.equalizer.low
                    , knobLabel "Low"
                    ]
                , div
                    [ cssClass KnobColumn ]
                    [ knob Mid model.equalizer.mid
                    , knobLabel "Mid"
                    ]
                , div
                    [ cssClass KnobColumn ]
                    [ knob High model.equalizer.high
                    , knobLabel "High"
                    ]
                ]
            ]
        ]



-- Knobs


knob : Knob -> Float -> Html TopLevel.Msg
knob knobType value =
    Html.map TopLevel.EqualizerMsg (knob_ knobType value)


knob_ : Knob -> Float -> Html Msg
knob_ knobType value =
    let
        angle =
            case knobType of
                Volume ->
                    (value * maxAngle * 2) - maxAngle

                _ ->
                    value * maxAngle

        styles =
            [ ( "transform", "rotate(" ++ (toString angle) ++ "deg)" )
            ]
    in
        div
            [ cssClass Knob
            , onDoubleClick
                (ResetKnob knobType)
            , onWithOptions
                "mousedown"
                { preventDefault = True
                , stopPropagation = True
                }
                (Decode.map (ActivateKnob knobType) Mouse.position)
            , onWithOptions
                "touchstart"
                { preventDefault = True
                , stopPropagation = True
                }
                (Equalizer.Touch.start knobType)
            ]
            [ div
                [ cssClass KnobLayerA ]
                []
            , div
                [ cssClass KnobLayerB, style styles ]
                [ decagonSvg ]
            , div
                [ cssClass KnobLayerC, style styles ]
                []
            , div
                [ cssClass KnobLines ]
                []
            ]


knobLabel : String -> Html TopLevel.Msg
knobLabel lbl =
    div
        [ cssClass KnobLabel ]
        [ text lbl ]



-- Helpers


backdropOffset : String
backdropOffset =
    (gr 7)
        |> .value
        |> (\r -> "calc(" ++ r ++ " + 1px)")


decagonSvg : Svg msg
decagonSvg =
    svg
        [ height "200", viewBox "0 0 200 200", width "200" ]
        [ polygon
            [ points "129.665631459995,191.301425564335 70.3343685400051,191.301425564335 22.3343685400051,156.427384220077 4,100 22.334368540005,43.5726157799226 70.334368540005,8.69857443566526 129.665631459995,8.69857443566525 177.665631459995,43.5726157799226 196,100 177.665631459995,156.427384220077" ]
            []
        ]
