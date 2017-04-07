module Console.Utils exposing (..)

import Console.Types exposing (..)
import Types as TopLevel exposing (Illumination)
import Utils


-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.ConsoleMsg
