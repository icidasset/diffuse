module Equalizer.State exposing (..)

import Equalizer.Ports exposing (..)
import Equalizer.Types exposing (..)
import Mouse
import Navigation
import Types as TopLevel


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    { low = flags.settings.equalizer.low
    , mid = flags.settings.equalizer.mid
    , high = flags.settings.equalizer.high
    , volume = flags.settings.equalizer.volume

    -- Knob interactions
    , activeKnob = Nothing
    , startingMousePosition = { x = 0, y = 0 }
    }


initialCommands : Cmd TopLevel.Msg
initialCommands =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        ------------------------------------
        -- Activate
        ------------------------------------
        ActivateKnob knob mousePos ->
            -- TODO: mousePos should be the center position of the knob,
            --       not the starting mouse position
            (!) { model | activeKnob = Just knob, startingMousePosition = mousePos } []

        ------------------------------------
        -- Adjust
        ------------------------------------
        AdjustKnob mousePos ->
            let
                start =
                    model.startingMousePosition

                end =
                    mousePos

                x =
                    toFloat (end.x - start.x)

                y =
                    toFloat (start.y - end.y)

                distance =
                    sqrt (x ^ 2 + y ^ 2)

                angle =
                    atan2 x y
                        * (180 / pi)
                        |> max (maxAngle * -1)
                        |> min maxAngle

                value =
                    if distance > 10 then
                        case model.activeKnob of
                            Just Volume ->
                                Just ( Volume, (maxAngle + angle) / (maxAngle * 2) )

                            Just knobType ->
                                Just ( knobType, angle / maxAngle )

                            Nothing ->
                                Nothing
                    else
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
                        (!) newModel [ adjustKnob knobType v ]

                    Nothing ->
                        (!) newModel []

        ------------------------------------
        -- Deactivate
        ------------------------------------
        DeactivateKnob ->
            (!)
                { model | activeKnob = Nothing }
                [ storeSettings model ]

        ------------------------------------
        -- Reset
        ------------------------------------
        ResetKnob Low ->
            reset { model | low = 0 } Low 0

        ResetKnob Mid ->
            reset { model | mid = 0 } Mid 0

        ResetKnob High ->
            reset { model | high = 0 } High 0

        ResetKnob Volume ->
            reset { model | volume = 0.5 } Volume 0.5


adjustKnob : Knob -> Float -> Cmd TopLevel.Msg
adjustKnob knobType value =
    adjustEqualizerSetting
        { knob = toString knobType
        , value = value
        }


maxAngle : Float
maxAngle =
    135


reset : Model -> Knob -> Float -> ( Model, Cmd TopLevel.Msg )
reset newModel knobType value =
    (!) newModel [ adjustKnob knobType value, storeSettings newModel ]


storeSettings : Model -> Cmd TopLevel.Msg
storeSettings model =
    storeEqualizerSettings
        { low = model.low
        , mid = model.mid
        , high = model.high
        , volume = model.volume
        }



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.activeKnob of
        Just _ ->
            Sub.batch
                [ Mouse.moves AdjustKnob
                , Mouse.ups (always DeactivateKnob)
                ]

        Nothing ->
            Sub.none
