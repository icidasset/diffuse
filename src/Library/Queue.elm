module Queue exposing (EngineItem, Item, makeEngineItem, makeItem, makeTrackUrl)

import Dict exposing (Dict)
import List.Extra as List
import Sources exposing (Source)
import Sources.Processing exposing (HttpMethod(..))
import Sources.Services
import Time
import Tracks exposing (IdentifiedTrack, Tags, Track)



-- 🌳


type alias Item =
    { manualEntry : Bool
    , identifiedTrack : IdentifiedTrack
    }


type alias EngineItem =
    { isCached : Bool
    , isPreload : Bool
    , progress : Maybe Float
    , sourceId : String
    , trackId : String
    , trackTags : Tags
    , trackPath : String
    , url : String
    }



-- 🔱


makeEngineItem : Bool -> Time.Posix -> List Source -> List String -> Dict String Float -> Track -> EngineItem
makeEngineItem preload timestamp sources cachedTrackIds progressTable track =
    { isCached = List.member track.id cachedTrackIds
    , isPreload = preload
    , progress = Dict.get track.id progressTable
    , sourceId = track.sourceId
    , trackId = track.id
    , trackPath = track.path
    , trackTags = track.tags
    , url = makeTrackUrl timestamp sources track
    }


makeItem : Bool -> IdentifiedTrack -> Item
makeItem isManualEntry identifiedTrack =
    { manualEntry = isManualEntry
    , identifiedTrack = identifiedTrack
    }


makeTrackUrl : Time.Posix -> List Source -> Track -> String
makeTrackUrl timestamp sources track =
    sources
        |> List.find (.id >> (==) track.sourceId)
        |> Maybe.map (makeTrackUrl_ timestamp track)
        |> Maybe.withDefault "<missing-source>"



-- ㊙️


makeTrackUrl_ : Time.Posix -> Track -> Source -> String
makeTrackUrl_ timestamp track source =
    Sources.Services.makeTrackUrl
        source.service
        timestamp
        source.id
        source.data
        Get
        track.path
