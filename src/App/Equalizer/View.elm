module Equalizer.View exposing (entry)

import Equalizer.State exposing (maxAngle)
import Equalizer.Types exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Material.Icons.Navigation as Icons
import Mouse
import Navigation.View as Navigation
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
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.inside
            [ ( span
                    []
                    [ Icons.arrow_back colorDerivatives.text 16
                    , label [] [ text "Tracks" ]
                    ]
              , "/"
              )
            ]

        ------------------------------------
        -- Content
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "EQ" ]
            , div
                [ cssClass LogoBackdrop
                , style [ ( "top", backdropOffset ), ( "bottom", "-1px" ) ]
                ]
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
            ]
            [ div
                [ cssClass KnobLayerA ]
                []
            , div
                [ cssClass KnobLayerB, style styles ]
                [ emptyDiv, emptyDiv, emptyDiv, emptyDiv, emptyDiv ]
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


emptyDiv : Html msg
emptyDiv =
    div [] []
