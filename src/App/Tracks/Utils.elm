module Tracks.Utils exposing (..)

import Tracks.Types exposing (..)
import Types as TopLevel exposing (Illumination)
import Utils


-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.TracksMsg



-- 🌱


getIdentifiers : IdentifiedTrack -> Identifiers
getIdentifiers =
    Tuple.first


unindentify : IdentifiedTrack -> Track
unindentify =
    Tuple.second
