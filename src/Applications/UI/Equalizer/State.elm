module UI.Equalizer.State exposing (..)

import Common exposing (Switch(..))
import Equalizer exposing (..)
import Return exposing (return)
import UI.Ports as Ports
import UI.Types exposing (..)



-- 📣


adjustVolume : Float -> Manager
adjustVolume volume model =
    let
        settings =
            model.eqSettings
    in
    return
        { model | eqSettings = { settings | volume = volume } }
        (adjustKnobUsingPort Volume volume)


toggleVolumeSlider : Switch -> Manager
toggleVolumeSlider switch model =
    case switch of
        On ->
            Return.singleton { model | showVolumeSlider = True }

        Off ->
            Return.singleton { model | showVolumeSlider = False }



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
    adjustKnobUsingPort Volume eqSettings.volume
