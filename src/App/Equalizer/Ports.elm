port module Equalizer.Ports exposing (..)

import Equalizer.Types exposing (..)


-- 💡


port adjustEqualizerSetting : KnobWithValue -> Cmd msg


port storeEqualizerSettings : Settings -> Cmd msg
