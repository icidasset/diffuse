module Tracks.Utils exposing (..)

import Dom.Scroll
import Task
import Tracks.Types exposing (..)
import Types as TopLevel exposing (Illumination)
import Utils


-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.TracksMsg


{-| Scroll tracks to top.
This should happen in combination with recalibration.
-}
recalibrationEffect : Cmd TopLevel.Msg
recalibrationEffect =
    Task.attempt (always TopLevel.NoOp) (Dom.Scroll.toTop "tracks")



-- 🌱


getIdentifiers : IdentifiedTrack -> Identifiers
getIdentifiers =
    Tuple.first


unindentify : IdentifiedTrack -> Track
unindentify =
    Tuple.second
