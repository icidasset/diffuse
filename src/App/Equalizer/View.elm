module Equalizer.View exposing (entry)

import Color.Convert exposing (colorToCssRgba)
import Color.Ext exposing (..)
import Equalizer.State exposing (maxAngle)
import Equalizer.Touch
import Equalizer.Types exposing (..)
import Json.Decode as Decode
import Material.Icons.Navigation as Icons
import Mouse
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Routing.Types
import Svg exposing (Svg, svg, polygon)
import Svg.Attributes
import Types as TopLevel
import Variables exposing (scaled)


-- Elements

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Types exposing (Node)
import Layouts
import Variations exposing (Variations)


-- Styles

import Equalizer.Styles exposing (..)
import Styles exposing (Styles(Equalizer, Zed))


-- 🍯


entry : TopLevel.Model -> Node
entry model =
    column
        Zed
        [ height fill ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideNew
            [ ( Icon Icons.arrow_back
              , Label (Hidden "Go back")
              , Routing.Types.Index
              )
            ]

        ------------------------------------
        -- Content
        ------------------------------------
        , column
            Zed
            [ height fill, paddingXY (scaled 4) 0 ]
            [ Layouts.h1 "EQ"
            , Layouts.logoBackdrop
            , row
                (Equalizer Wrapper)
                [ spread ]
                [ column
                    (Equalizer Column)
                    [ paddingXY (scaled 10) (scaled -2) ]
                    [ knob Volume model.equalizer.volume
                    , knobLines
                    , knobLabel "Volume"
                    ]
                , column
                    (Equalizer Column)
                    [ paddingXY (scaled 10) (scaled -2) ]
                    [ knob Low model.equalizer.low
                    , knobLines
                    , knobLabel "Low"
                    ]
                , column
                    (Equalizer Column)
                    [ paddingXY (scaled 10) (scaled -2) ]
                    [ knob Mid model.equalizer.mid
                    , knobLines
                    , knobLabel "Mid"
                    ]
                , column
                    (Equalizer Column)
                    [ paddingXY (scaled 10) (scaled -2) ]
                    [ knob High model.equalizer.high
                    , knobLines
                    , knobLabel "High"
                    ]
                ]
                |> el Zed [ center, verticalCenter ]
                |> el Zed [ height fill ]
            ]
        ]



-- Knobs


knob : Knob -> Float -> Node
knob knobType value =
    value
        |> knob_ knobType
        |> Element.map TopLevel.EqualizerMsg


knob_ : Knob -> Float -> Element Styles.Styles Variations Msg
knob_ knobType value =
    let
        angle =
            case knobType of
                Volume ->
                    (value * maxAngle * 2) - maxAngle

                _ ->
                    value * maxAngle

        styles =
            [ ( "-webkit-transform", "rotate(" ++ (toString angle) ++ "deg)" )
            , ( "transform", "rotate(" ++ (toString angle) ++ "deg)" )
            ]

        activateKnob =
            Decode.map (ActivateKnob knobType) Mouse.position
    in
        within
            [ -- Decagon
              --
              el
                Zed
                [ height fill, padding 4, width fill ]
                (html decagonSvg)

            -- Layer A
            --
            , el
                Zed
                [ height fill, padding 8, width fill ]
                (el
                    (Equalizer LayerA)
                    [ height fill, width fill ]
                    empty
                )

            -- Layer B
            --
            , el
                Zed
                [ height fill, padding 8, width fill ]
                (el
                    (Equalizer LayerB)
                    [ height (px 9), center, width (px 2) ]
                    empty
                )
            ]
            (el
                (Equalizer Knob)
                [ height (px knobSize)
                , width (px knobSize)

                --
                , inlineStyle styles

                --
                , onDoubleClick (ResetKnob knobType)
                , onWithOptions "mousedown" preventAndStop activateKnob
                , onWithOptions "touchstart" preventAndStop (Equalizer.Touch.start knobType)
                ]
                empty
            )


knobLabel : String -> Node
knobLabel lbl =
    el
        (Equalizer KnobLabel)
        []
        (text lbl)


knobLines : Node
knobLines =
    row
        Zed
        [ center ]
        [ -- Left
          --
          el
            (Equalizer Line)
            [ height (px 9)
            , width (px 1)

            --
            , moveLeft 17
            , moveUp 8

            --
            , inlineStyle
                [ ( "-webkit-transform", "rotate(45deg)" )
                , ( "transform", "rotate(45deg)" )
                ]
            ]
            empty
        , -- Right
          --
          el
            (Equalizer Line)
            [ height (px 9)
            , width (px 1)

            --
            , moveRight 17
            , moveUp 8

            --
            , inlineStyle
                [ ( "-webkit-transform", "rotate(-45deg)" )
                , ( "transform", "rotate(-45deg)" )
                ]
            ]
            empty
        ]



-- Helpers


decagonSvg : Svg msg
decagonSvg =
    svg
        [ Svg.Attributes.fill "transparent"
        , Svg.Attributes.height "200"
        , Svg.Attributes.stroke (knobColor |> setAlpha knobOpacity |> colorToCssRgba)
        , Svg.Attributes.strokeLinejoin "miter"
        , Svg.Attributes.strokeWidth "7px"
        , Svg.Attributes.style "height: 100%; width: 100%;"
        , Svg.Attributes.viewBox "0 0 200 200"
        , Svg.Attributes.width "200"
        ]
        [ polygon
            [ Svg.Attributes.points "129.665631459995,191.301425564335 70.3343685400051,191.301425564335 22.3343685400051,156.427384220077 4,100 22.334368540005,43.5726157799226 70.334368540005,8.69857443566526 129.665631459995,8.69857443566525 177.665631459995,43.5726157799226 196,100 177.665631459995,156.427384220077" ]
            []
        ]


preventAndStop : Options
preventAndStop =
    { preventDefault = True
    , stopPropagation = True
    }
