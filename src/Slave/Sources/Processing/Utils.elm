module Sources.Processing.Utils exposing (..)

import Slave.Types exposing (Illumination)
import Sources.Processing.Types exposing (..)
import Utils


-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate Slave.Types.SourceProcessingMsg
