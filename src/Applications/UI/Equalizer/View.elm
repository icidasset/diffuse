module UI.Equalizer.View exposing (view)

import Chunky exposing (..)
import Chunky.Styled
import Color exposing (Color)
import Color.Ext as Color
import Common
import Css
import Css.Classes as C
import Css.Ext as Css
import Equalizer exposing (..)
import Html exposing (Html)
import Html.Events.Extra.Pointer as Pointer
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Json.Decode as Decode
import Material.Icons as Icons
import Svg.Styled
import Svg.Styled.Attributes
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
            [ C.relative, C.select_none ]
            [ chunk
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
    chunk
        [ C.text_center ]
        [ chunk
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
        , chunk
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
    [ knob_ knobType value
    , knobLines
    , knobLabel knobType
    ]
        |> Chunky.Styled.chunk
            [ C.border_black_05
            , C.border_r
            , C.flex_grow
            , C.flex_shrink_0
            , C.px_16
            , C.py_4

            --
            , C.last__border_r_0
            ]
        |> Html.Styled.toUnstyled


knob_ : Knob -> Float -> Html.Styled.Html Msg
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
    Chunky.Styled.brick
        [ css knobStyles

        --
        , [ "rotate(", String.fromFloat angle, "deg)" ]
            |> String.concat
            |> Html.Styled.Attributes.style "transform"

        --
        , Html.Styled.Events.custom "dblclick" resetDecoder
        , Html.Styled.Events.custom "dbltap" resetDecoder

        --
        , knobType
            |> ActivateKnob
            |> Pointer.onDown
            |> Html.Styled.Attributes.fromUnstyled
        ]
        [ C.cursor_pointer
        , C.mx_auto
        , C.overflow_hidden
        , C.relative
        , C.rounded_full
        ]
        [ Html.Styled.map never knob__ ]


knob__ : Html.Styled.Html Never
knob__ =
    Chunky.Styled.raw
        [ decagonSvg
        , Chunky.Styled.brick
            [ css layerAStyles ]
            [ C.absolute, C.inset_0, C.rounded_full, C.z_10 ]
            [ Chunky.Styled.brick
                [ css layerBStyles ]
                [ C.mx_auto ]
                []
            ]
        ]


knobLabel : Knob -> Html.Styled.Html Msg
knobLabel knobType =
    Chunky.Styled.brick
        [ css knobLabelStyles ]
        [ C.font_semibold
        , C.mt_3
        , C.opacity_70
        , C.tracking_wide
        ]
        [ case knobType of
            Low ->
                Html.Styled.text "LOW"

            Mid ->
                Html.Styled.text "MID"

            High ->
                Html.Styled.text "HIGH"

            Volume ->
                Html.Styled.text "VOLUME"
        ]


knobLines : Html.Styled.Html Msg
knobLines =
    Chunky.Styled.brick
        [ css knobLineContainerStyles ]
        [ C.mx_auto, C.relative ]
        [ Chunky.Styled.brick
            [ css (knobLineStyles 45) ]
            [ C.absolute, C.left_0, C.top_0 ]
            []
        , Chunky.Styled.brick
            [ css (knobLineStyles -45) ]
            [ C.absolute, C.right_0, C.top_0 ]
            []
        ]



-- VARIABLES


knobColor : Color
knobColor =
    UI.Kit.colorKit.base03


knobOpacity : Float
knobOpacity =
    0.7


knobSize : Float
knobSize =
    36



-- 🖼


knobStyles : List Css.Style
knobStyles =
    [ Css.height (Css.px knobSize)
    , Css.width (Css.px knobSize)

    --
    , Css.boxShadow6
        Css.inset
        Css.zero
        Css.zero
        (Css.px 5)
        (Css.px 1)
        (knobColor
            |> Color.setOpacity (knobOpacity - 0.35)
            |> Color.toElmCssColor
        )
    ]


knobLabelStyles : List Css.Style
knobLabelStyles =
    [ Css.fontSize (Css.px 9.5)
    ]


knobLineContainerStyles : List Css.Style
knobLineContainerStyles =
    [ Css.top (Css.px <| knobSize / -4.5)
    , Css.width (Css.px knobSize)
    ]


knobLineStyles : Float -> List Css.Style
knobLineStyles rotation =
    [ Css.height (Css.px 9)
    , Css.transform (Css.rotate <| Css.deg rotation)
    , Css.width (Css.px 1)

    --
    , knobColor
        |> Color.setOpacity (knobOpacity + 0.1)
        |> Color.toElmCssColor
        |> Css.backgroundColor
    ]


layerAStyles : List Css.Style
layerAStyles =
    [ Css.margin (Css.px 8)

    --
    , Css.boxShadow5 Css.zero
        Css.zero
        (Css.px 6)
        (Css.px 1)
        (knobColor
            |> Color.setOpacity (knobOpacity + 0.3)
            |> Color.toElmCssColor
        )
    ]


layerBStyles : List Css.Style
layerBStyles =
    [ Css.height (Css.px 9)
    , Css.width (Css.px 2)

    --
    , knobColor
        |> Color.setOpacity (knobOpacity + 0.1)
        |> Color.toElmCssColor
        |> Css.backgroundColor
    ]



-- DECAGON


decagonSvg : Svg.Styled.Svg msg
decagonSvg =
    Svg.Styled.svg
        [ Svg.Styled.Attributes.class C.mx_auto
        , Svg.Styled.Attributes.css decagonStyles
        , Svg.Styled.Attributes.fill "transparent"
        , Svg.Styled.Attributes.height "200"
        , Svg.Styled.Attributes.stroke (knobColor |> Color.setOpacity knobOpacity |> Color.toCssString)
        , Svg.Styled.Attributes.strokeLinejoin "miter"
        , Svg.Styled.Attributes.strokeWidth "7px"
        , Svg.Styled.Attributes.viewBox "0 0 200 200"
        , Svg.Styled.Attributes.width "200"
        ]
        [ Svg.Styled.polygon
            [ Svg.Styled.Attributes.points "129.665631459995,191.301425564335 70.3343685400051,191.301425564335 22.3343685400051,156.427384220077 4,100 22.334368540005,43.5726157799226 70.334368540005,8.69857443566526 129.665631459995,8.69857443566525 177.665631459995,43.5726157799226 196,100 177.665631459995,156.427384220077" ]
            []
        ]


decagonStyles : List Css.Style
decagonStyles =
    [ Css.height <| Css.calc (Css.pct 100) Css.minus (Css.px 8)
    , Css.width <| Css.calc (Css.pct 100) Css.minus (Css.px 8)

    --
    , Css.marginTop (Css.px 4)
    ]
