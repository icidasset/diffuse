module UI.Sources.State exposing (..)

import Monocle.Lens as Lens exposing (Lens)



-- 🌳


lens =
    { get = .sources
    , set = \sources m -> { m | sources = sources }
    }


formLens =
    Lens.compose
        lens
        { get = .form
        , set = \form m -> { m | form = form }
        }
