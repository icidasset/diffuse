module UI.Equalizer.View exposing (view)

import Common
import Css.Classes as C
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
        , Html.div
            [ C.relative, C.select_none ]
            [ Html.div
                [ C.absolute, C.left_0, C.top_0 ]
                [ UI.Kit.canister [ UI.Kit.h1 "Equalizer" ]
                ]
            ]

        --
        , UI.Kit.centeredContent
            [ eqView settings ]
        ]


eqView : Settings -> Html Msg
eqView settings =
    Html.div
        [ A.class "equalizer"
        , C.text_center
        ]
        [ Html.div
            [ C.border
            , C.border_black_05
            , C.rounded
            , C.flex

            -- Dark mode
            ------------
            , C.dark__border_base00
            ]
            [ knob Volume settings.volume
            ]

        --
        , Html.div
            [ C.border
            , C.border_black_05
            , C.rounded
            , C.flex
            , C.mt_4

            -- Dark mode
            ------------
            , C.dark__border_base00
            ]
            [ knob Low settings.low
            , knob Mid settings.mid
            , knob High settings.high
            ]
        ]



-- KNOB


knob : Knob -> Float -> Html Msg
knob knobType value =
    Html.div
        [ C.border_black_05
        , C.border_r
        , C.flex_grow
        , C.flex_shrink_0
        , C.px_10
        , C.py_4

        --
        , C.last__border_r_0

        --
        , C.sm__px_12
        , C.md__px_16
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
    Html.div
        [ A.class "knob"
        , A.style "transform" ("rotate(" ++ String.fromFloat angle ++ "deg)")

        --
        , Html.Events.custom "dblclick" resetDecoder
        , Html.Events.custom "dbltap" resetDecoder

        --
        , knobType
            |> ActivateKnob
            |> Pointer.onDown

        --
        , C.cursor_pointer
        , C.mx_auto
        , C.overflow_hidden
        , C.relative
        , C.rounded_full
        ]
        [ Html.map never knob__ ]


knob__ : Html Never
knob__ =
    Html.div
        []
        [ decagonSvg
        , Html.div
            [ A.class "layer-a"
            , C.absolute
            , C.inset_0
            , C.rounded_full
            , C.z_10
            ]
            [ Html.div
                [ A.class "layer-b"
                , C.mx_auto
                ]
                []
            ]
        ]


knobLabel : Knob -> Html Msg
knobLabel knobType =
    Html.div
        [ A.class "knob-label"

        --
        , C.font_semibold
        , C.mt_3
        , C.opacity_70
        , C.tracking_wide
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
    Html.div
        [ A.class "knob-lines"
        , C.mx_auto
        , C.relative
        ]
        [ Html.div [ C.absolute, C.left_0, C.top_0 ] []
        , Html.div [ C.absolute, C.right_0, C.top_0 ] []
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
