module Queue exposing (EngineItem, Item)

import Tracks exposing (IdentifiedTrack, Track)



-- 🌳


type alias Item =
    { manualEntry : Bool
    , identifiedTrack : IdentifiedTrack
    }


type alias EngineItem =
    { track : Track
    , url : String
    }
