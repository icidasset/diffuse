module UI.Queue.State exposing (..)

import Monocle.Lens as Lens exposing (Lens)



-- 🌳


lens =
    { get = .queue
    , set = \queue ui -> { ui | queue = queue }
    }
