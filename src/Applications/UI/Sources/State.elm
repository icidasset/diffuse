module UI.Sources.State exposing (..)

import Monocle.Lens as Lens exposing (Lens)



-- 🌳


lens =
    { get = .sources
    , set = \sources ui -> { ui | sources = sources }
    }
