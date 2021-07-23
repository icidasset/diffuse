module UI.Equalizer.View exposing (view)

import Chunky exposing (..)
import Common
import Equalizer exposing (..)
import Html exposing (Html)
import Html.Attributes as A
import Html.Events
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as Decode
import Material.Icons as Icons
import Svg
import Svg.Attributes
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page
import UI.Types exposing (Msg(..))



-- 🗺


view : Settings -> Html Msg
view settings =
    UI.Kit.receptacle
        { scrolling = True }
        [ -----------------------------------------
          -- Navigation
          -----------------------------------------
          UI.Navigation.local
            [ ( Icon Icons.arrow_back
              , Label Common.backToIndex Hidden
              , NavigateToPage UI.Page.Index
              )
            ]

        -----------------------------------------
        -- Content
        -----------------------------------------
        , chunk
            [ "relative", "select-none" ]
            [ chunk
                [ "absolute", "left-0", "top-0" ]
                [ UI.Kit.canister [ UI.Kit.h1 "Equalizer" ]
                ]
            ]

        --
        , UI.Kit.centeredContent
            [ eqView settings ]
        ]


eqView : Settings -> Html Msg
eqView settings =
    chunk
        [ "equalizer"

        --
        , "text-center"
        ]
        [ chunk
            [ "border"
            , "border-black-05"
            , "rounded"
            , "flex"

            -- Dark mode
            ------------
            , "dark:border-base00"
            ]
            [ knob Volume settings.volume
            ]

        --
        , chunk
            [ "border"
            , "border-black-05"
            , "rounded"
            , "flex"
            , "mt-4"

            -- Dark mode
            ------------
            , "dark:border-base00"
            ]
            [ knob Low settings.low
            , knob Mid settings.mid
            , knob High settings.high
            ]
        ]



-- KNOB


knob : Knob -> Float -> Html Msg
knob knobType value =
    chunk
        [ "border-black-05"
        , "border-r"
        , "flex-grow"
        , "flex-shrink-0"
        , "px-10"
        , "py-4"

        --
        , "last:border-r-0"

        --
        , "sm:px-12"
        , "md:px-16"
        ]
        [ knob_ knobType value
        , knobLines
        , knobLabel knobType
        ]


knob_ : Knob -> Float -> Html Msg
knob_ knobType value =
    let
        angle =
            case knobType of
                Volume ->
                    (value * maxAngle * 2) - maxAngle

                _ ->
                    value * maxAngle

        resetDecoder =
            Decode.succeed
                { message = ResetKnob knobType
                , stopPropagation = True
                , preventDefault = True
                }
    in
    brick
        [ A.style "transform" ("rotate(" ++ String.fromFloat angle ++ "deg)")

        --
        , Html.Events.custom "dblclick" resetDecoder
        , Html.Events.custom "dbltap" resetDecoder

        --
        , knobType
            |> ActivateKnob
            |> Pointer.onDown
        ]
        [ "knob"

        --
        , "cursor-pointer"
        , "mx-auto"
        , "overflow-hidden"
        , "relative"
        , "rounded-full"
        ]
        [ Html.map never knob__ ]


knob__ : Html Never
knob__ =
    Html.div
        []
        [ decagonSvg
        , chunk
            [ "layer-a"

            --
            , "absolute"
            , "inset-0"
            , "rounded-full"
            , "z-10"
            ]
            [ chunk
                [ "layer-b"

                --
                , "mx-auto"
                ]
                []
            ]
        ]


knobLabel : Knob -> Html Msg
knobLabel knobType =
    chunk
        [ "knob-label"

        --
        , "font-semibold"
        , "mt-3"
        , "opacity-70"
        , "tracking-wide"
        ]
        [ case knobType of
            Low ->
                Html.text "LOW"

            Mid ->
                Html.text "MID"

            High ->
                Html.text "HIGH"

            Volume ->
                Html.text "VOLUME"
        ]


knobLines : Html Msg
knobLines =
    chunk
        [ "knob-lines"

        --
        , "mx-auto"
        , "relative"
        ]
        [ chunk [ "absolute", "left-0", "top-0" ] []
        , chunk [ "absolute", "right-0", "top-0" ] []
        ]



-- DECAGON


decagonSvg : Svg.Svg msg
decagonSvg =
    Svg.svg
        [ Svg.Attributes.class "mx-auto"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.height "200"
        , Svg.Attributes.strokeLinejoin "miter"
        , Svg.Attributes.strokeWidth "7px"
        , Svg.Attributes.viewBox "0 0 200 200"
        , Svg.Attributes.width "200"
        ]
        [ Svg.polygon
            [ Svg.Attributes.points "129.665631459995,191.301425564335 70.3343685400051,191.301425564335 22.3343685400051,156.427384220077 4,100 22.334368540005,43.5726157799226 70.334368540005,8.69857443566526 129.665631459995,8.69857443566525 177.665631459995,43.5726157799226 196,100 177.665631459995,156.427384220077" ]
            []
        ]
