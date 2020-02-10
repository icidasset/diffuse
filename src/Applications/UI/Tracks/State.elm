module UI.Tracks.State exposing (..)

import Monocle.Lens as Lens exposing (Lens)



-- 🌳


lens =
    { get = .tracks
    , set = \tracks ui -> { ui | tracks = tracks }
    }
