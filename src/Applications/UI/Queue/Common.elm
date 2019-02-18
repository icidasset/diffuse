module UI.Queue.Common exposing (makeEngineItem, makeItem)

import List.Extra as List
import Queue exposing (..)
import Sources exposing (Source)
import Sources.Processing exposing (HttpMethod(..))
import Sources.Services
import Time
import Tracks exposing (IdentifiedTrack, Track)



-- 🔱


makeEngineItem : Time.Posix -> List Source -> IdentifiedTrack -> EngineItem
makeEngineItem timestamp sources ( _, track ) =
    { track = track
    , url =
        sources
            |> List.find (.id >> (==) track.sourceId)
            |> Maybe.map (makeTrackUrl timestamp track)
            |> Maybe.withDefault "<missing-source>"
    }


makeItem : Bool -> IdentifiedTrack -> Item
makeItem isManualEntry identifiedTrack =
    { manualEntry = isManualEntry
    , identifiedTrack = identifiedTrack
    }



-- ㊙️


makeTrackUrl : Time.Posix -> Track -> Source -> String
makeTrackUrl timestamp track source =
    Sources.Services.makeTrackUrl
        source.service
        timestamp
        source.data
        Get
        track.path
