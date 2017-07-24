module Equalizer.State exposing (..)

import Equalizer.Types exposing (..)
import Mouse
import Navigation
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { low = 0
    , mid = 0
    , high = 0
    , volume = 1

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
                    if distance >= 15 then
                        case model.activeKnob of
                            Just Volume ->
                                Just ( Volume, (maxAngle + angle) / (maxAngle * 2) )

                            Just knobType ->
                                Just ( knobType, angle / maxAngle )

                            Nothing ->
                                Nothing
                    else
                        Nothing
            in
                case value of
                    Just ( Low, v ) ->
                        (!) { model | low = v } []

                    Just ( Mid, v ) ->
                        (!) { model | mid = v } []

                    Just ( High, v ) ->
                        (!) { model | high = v } []

                    Just ( Volume, v ) ->
                        (!) { model | volume = v } []

                    Nothing ->
                        (!) model []

        ------------------------------------
        -- Deactivate
        ------------------------------------
        DeactivateKnob ->
            (!) { model | activeKnob = Nothing } []

        ------------------------------------
        -- Reset
        ------------------------------------
        ResetKnob Low ->
            (!) { model | low = 0 } []

        ResetKnob Mid ->
            (!) { model | mid = 0 } []

        ResetKnob High ->
            (!) { model | high = 0 } []

        ResetKnob Volume ->
            (!) { model | volume = 0.5 } []


maxAngle : Float
maxAngle =
    135



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
