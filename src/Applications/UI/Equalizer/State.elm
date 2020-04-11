module UI.Equalizer.State exposing (..)

import Coordinates
import Equalizer exposing (..)
import Html.Events.Extra.Pointer as Pointer
import Return exposing (andThen, return)
import UI.Ports as Ports
import UI.Types as UI exposing (..)
import UI.User.State.Export as User



-- 📣


activateKnob : Knob -> Pointer.Event -> Manager
activateKnob theKnob { pointer } model =
    { knob = theKnob
    , startingPosition = Coordinates.fromTuple pointer.clientPos
    }
        |> (\m -> { model | eqKnobOperation = Just m })
        |> Return.singleton


adjustKnob : Pointer.Event -> Manager
adjustKnob { pointer } model =
    let
        start =
            case model.eqKnobOperation of
                Just { startingPosition } ->
                    startingPosition

                Nothing ->
                    { x = 0, y = 0 }

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
            case ( distance > 10, Maybe.map .knob model.eqKnobOperation ) of
                ( True, Just Volume ) ->
                    Just ( Volume, (maxAngle + angle) / (maxAngle * 2) )

                ( True, Just knobType ) ->
                    Just ( knobType, angle / maxAngle )

                _ ->
                    Nothing

        settings =
            model.eqSettings

        newSettings =
            case value of
                Just ( Low, v ) ->
                    { settings | low = v }

                Just ( Mid, v ) ->
                    { settings | mid = v }

                Just ( High, v ) ->
                    { settings | high = v }

                Just ( Volume, v ) ->
                    { settings | volume = v }

                Nothing ->
                    settings
    in
    case value of
        Just ( knobType, v ) ->
            return { model | eqSettings = newSettings } (adjustKnobUsingPort knobType v)

        Nothing ->
            Return.singleton { model | eqSettings = newSettings }


deactivateKnob : Manager
deactivateKnob model =
    User.saveEnclosedUserData { model | eqKnobOperation = Nothing }


resetKnob : Knob -> Manager
resetKnob knob model =
    model
        |> resetKnobOrganizer knob
        |> andThen User.saveEnclosedUserData



-- ⚗️


adjustKnobUsingPort : Knob -> Float -> Cmd Msg
adjustKnobUsingPort knobType value =
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


adjustAllKnobs : Settings -> Cmd Msg
adjustAllKnobs eqSettings =
    Cmd.batch
        [ adjustKnobUsingPort Low eqSettings.low
        , adjustKnobUsingPort Mid eqSettings.mid
        , adjustKnobUsingPort High eqSettings.high
        , adjustKnobUsingPort Volume eqSettings.volume
        ]



-- ㊙️


resetKnobOrganizer : Knob -> Manager
resetKnobOrganizer knob model =
    let
        d =
            defaultSettings

        s =
            model.eqSettings
    in
    case knob of
        Low ->
            reset Low { s | low = d.low } d.low model

        Mid ->
            reset Mid { s | mid = d.mid } d.mid model

        High ->
            reset High { s | high = d.high } d.high model

        Volume ->
            reset Volume { s | volume = d.volume } d.volume model


reset : Knob -> Equalizer.Settings -> Float -> Manager
reset knobType newSettings value model =
    ( { model | eqSettings = newSettings }
    , adjustKnobUsingPort knobType value
    )
