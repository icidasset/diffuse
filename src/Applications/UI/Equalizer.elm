module UI.Equalizer exposing (Model, Msg(..), adjustAllKnobs, initialModel, update, view)

import Chunky exposing (..)
import Color exposing (Color)
import Color.Ext as Color
import Common
import Coordinates exposing (Coordinates)
import Css
import Css.Ext as Css
import Equalizer exposing (..)
import Html.Events.Extra.Pointer as Pointer
import Html.Styled exposing (Html, text)
import Html.Styled.Attributes exposing (css, style)
import Html.Styled.Events
import Material.Icons.Navigation as Icons
import Return3 as Return exposing (..)
import Svg.Styled exposing (Svg, polygon, svg)
import Svg.Styled.Attributes
import Tachyons.Classes as T
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page
import UI.Ports as Ports
import UI.Reply exposing (Reply(..))



-- 🌳


type Knob
    = Low
    | Mid
    | High
    | Volume


type alias Model =
    { low : Float
    , mid : Float
    , high : Float
    , volume : Float

    --
    , activeKnob : Maybe Knob
    , startCoordinates : Coordinates
    }


initialModel : Model
initialModel =
    { low = defaultSettings.low
    , mid = defaultSettings.mid
    , high = defaultSettings.high
    , volume = defaultSettings.volume

    -- Knob interactions
    --------------------
    , activeKnob = Nothing
    , startCoordinates = { x = 0, y = 0 }
    }



-- 📣


type Msg
    = ActivateKnob Knob Pointer.Event
    | AdjustKnob Pointer.Event
    | DeactivateKnob Pointer.Event
    | ResetKnob Knob


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        -----------------------------------------
        -- Activate
        -----------------------------------------
        ActivateKnob theKnob { pointer } ->
            { model
                | activeKnob = Just theKnob
                , startCoordinates = Coordinates.fromTuple pointer.clientPos
            }
                |> return

        -----------------------------------------
        -- Adjust
        -----------------------------------------
        AdjustKnob { pointer } ->
            let
                start =
                    model.startCoordinates

                end =
                    (\( a, b ) -> { x = a, y = b })
                        pointer.clientPos

                x =
                    end.x - start.x

                y =
                    start.y - end.y

                distance =
                    sqrt (x ^ 2 + y ^ 2)

                angle =
                    atan2 x y
                        * (180 / pi)
                        |> max (maxAngle * -1)
                        |> min maxAngle

                value =
                    case ( distance > 10, model.activeKnob ) of
                        ( True, Just Volume ) ->
                            Just ( Volume, (maxAngle + angle) / (maxAngle * 2) )

                        ( True, Just knobType ) ->
                            Just ( knobType, angle / maxAngle )

                        _ ->
                            Nothing

                newModel =
                    case value of
                        Just ( Low, v ) ->
                            { model | low = v }

                        Just ( Mid, v ) ->
                            { model | mid = v }

                        Just ( High, v ) ->
                            { model | high = v }

                        Just ( Volume, v ) ->
                            { model | volume = v }

                        Nothing ->
                            model
            in
            case value of
                Just ( knobType, v ) ->
                    returnCommandWithModel newModel (adjustKnob knobType v)

                Nothing ->
                    return newModel

        -----------------------------------------
        -- Deactivate
        -----------------------------------------
        DeactivateKnob _ ->
            Return.replyWithModel
                { model | activeKnob = Nothing }
                SaveEnclosedUserData

        -----------------------------------------
        -- Reset
        -----------------------------------------
        ResetKnob Low ->
            reset { model | low = defaultSettings.low } Low defaultSettings.low

        ResetKnob Mid ->
            reset { model | mid = defaultSettings.mid } Mid defaultSettings.mid

        ResetKnob High ->
            reset { model | high = defaultSettings.high } High defaultSettings.high

        ResetKnob Volume ->
            reset { model | volume = defaultSettings.volume } Volume defaultSettings.volume



-- 📣  ░░  KNOBS & THINGS


adjustKnob : Knob -> Float -> Cmd Msg
adjustKnob knobType value =
    Ports.adjustEqualizerSetting
        { value = value
        , knob =
            case knobType of
                Low ->
                    "LOW"

                Mid ->
                    "MID"

                High ->
                    "HIGH"

                Volume ->
                    "VOLUME"
        }


adjustAllKnobs : Model -> Cmd Msg
adjustAllKnobs model =
    Cmd.batch
        [ adjustKnob Low model.low
        , adjustKnob Mid model.mid
        , adjustKnob High model.high
        , adjustKnob Volume model.volume
        ]


reset : Model -> Knob -> Float -> Return Model Msg Reply
reset newModel knobType value =
    ( newModel
    , adjustKnob knobType value
    , [ SaveEnclosedUserData ]
    )



-- 🗺


view : Model -> Html Msg
view model =
    UI.Kit.receptacle
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
        , brick
            [ css eqStyles ]
            [ T.relative ]
            [ chunk
                [ T.absolute, T.left_0, T.top_0 ]
                [ UI.Kit.canister [ UI.Kit.h1 "Equalizer" ]
                ]
            ]

        --
        , UI.Kit.centeredContent
            [ eqView model ]
        ]


eqView : Model -> Html Msg
eqView model =
    chunk
        [ T.tc ]
        [ brick
            [ css groupStyles ]
            [ T.ba, T.br2, T.flex ]
            [ knob Volume model.volume
            ]

        --
        , brick
            [ css groupStyles ]
            [ T.ba, T.br2, T.flex, T.mt3 ]
            [ knob Low model.low
            , knob Mid model.mid
            , knob High model.high
            ]
        ]



-- KNOB


knob : Knob -> Float -> Html Msg
knob knobType value =
    brick
        [ css columnStyles ]
        [ T.flex_grow_1, T.flex_shrink_0, T.ph4, T.ph5_ns, T.pv3 ]
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
    in
    brick
        [ css knobStyles

        --
        , [ "rotate(", String.fromFloat angle, "deg)" ]
            |> String.concat
            |> style "transform"

        --
        , Html.Styled.Events.onDoubleClick (ResetKnob knobType)

        --
        , knobType
            |> ActivateKnob
            |> Pointer.onDown
            |> Html.Styled.Attributes.fromUnstyled
        ]
        [ T.br_100, T.center, T.pointer, T.relative ]
        [ Html.Styled.map never knob__ ]


knob__ : Html Never
knob__ =
    raw
        [ decagonSvg
        , brick
            [ css layerAStyles ]
            [ T.absolute, T.absolute__fill, T.br_100, T.z_1 ]
            [ brick
                [ css layerBStyles ]
                [ T.center ]
                []
            ]
        ]


knobLabel : Knob -> Html Msg
knobLabel knobType =
    brick
        [ css knobLabelStyles ]
        [ T.fw6, T.mt3, T.o_70 ]
        [ case knobType of
            Low ->
                text "LOW"

            Mid ->
                text "MID"

            High ->
                text "HIGH"

            Volume ->
                text "VOLUME"
        ]


knobLines : Html Msg
knobLines =
    brick
        [ css knobLineContainerStyles ]
        [ T.center, T.relative ]
        [ brick
            [ css (knobLineStyles 45) ]
            [ T.absolute, T.left_0, T.top_0 ]
            []
        , brick
            [ css (knobLineStyles -45) ]
            [ T.absolute, T.right_0, T.top_0 ]
            []
        ]



-- VARIABLES


borderColor : Css.Color
borderColor =
    Css.rgba 0 0 0 0.075


knobColor : Color
knobColor =
    UI.Kit.colorKit.base03


knobOpacity : Float
knobOpacity =
    0.7


knobSize : Float
knobSize =
    36


maxAngle : Float
maxAngle =
    135



-- 🖼


eqStyles : List Css.Style
eqStyles =
    [ Css.disableUserSelection ]


columnStyles : List Css.Style
columnStyles =
    [ Css.borderRight3 (Css.px 1) Css.solid borderColor
    , Css.lastChild [ Css.borderRightWidth Css.zero ]
    ]


groupStyles : List Css.Style
groupStyles =
    [ Css.borderColor borderColor ]


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
    , Css.letterSpacing (Css.em 0.025)
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


decagonSvg : Svg msg
decagonSvg =
    svg
        [ Svg.Styled.Attributes.css decagonStyles
        , Svg.Styled.Attributes.fill "transparent"
        , Svg.Styled.Attributes.height "200"
        , Svg.Styled.Attributes.stroke (knobColor |> Color.setOpacity knobOpacity |> Color.toCssString)
        , Svg.Styled.Attributes.strokeLinejoin "miter"
        , Svg.Styled.Attributes.strokeWidth "7px"
        , Svg.Styled.Attributes.viewBox "0 0 200 200"
        , Svg.Styled.Attributes.width "200"
        ]
        [ polygon
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
