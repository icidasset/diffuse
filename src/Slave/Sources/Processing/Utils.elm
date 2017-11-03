module Sources.Processing.Utils exposing (..)

import Slave.Types exposing (..)
import Sources.Processing.Types
import Utils


-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate SourceProcessingMsg
