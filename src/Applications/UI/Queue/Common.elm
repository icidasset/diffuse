module UI.Queue.Common exposing (makeEngineItem, makeItem, makeTrackUrl)

import List.Extra as List
import Queue exposing (..)
import Sources exposing (Source)
import Sources.Processing exposing (HttpMethod(..))
import Sources.Services
import Time
import Tracks exposing (IdentifiedTrack, Track)



-- 🔱


makeEngineItem : Time.Posix -> List Source -> List String -> IdentifiedTrack -> EngineItem
makeEngineItem timestamp sources cachedTrackIds (( _, t ) as track) =
    { isCached = List.member t.id cachedTrackIds
    , trackId = t.id
    , url = makeTrackUrl timestamp sources track
    }


makeItem : Bool -> IdentifiedTrack -> Item
makeItem isManualEntry identifiedTrack =
    { manualEntry = isManualEntry
    , identifiedTrack = identifiedTrack
    }


makeTrackUrl : Time.Posix -> List Source -> IdentifiedTrack -> String
makeTrackUrl timestamp sources ( _, track ) =
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
        source.data
        Get
        track.path
