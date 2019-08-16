module Queue exposing (EngineItem, Item)

import Tracks exposing (IdentifiedTrack, Tags, Track)



-- 🌳


type alias Item =
    { manualEntry : Bool
    , identifiedTrack : IdentifiedTrack
    }


type alias EngineItem =
    { isCached : Bool
    , progress : Maybe Float
    , trackId : String
    , trackTags : Tags
    , url : String
    }
