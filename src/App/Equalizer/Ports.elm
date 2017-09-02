port module Equalizer.Ports exposing (..)

import Equalizer.Types exposing (..)


-- 💡


port adjustEqualizerSetting : KnobWithValue -> Cmd msg
