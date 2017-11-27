module Queue.Utils exposing (($), makeItem, makeEngineItem)

import Date exposing (Date)
import List.Extra as List
import Queue.Types exposing (..)
import Sources.Processing.Types exposing (HttpMethod(..))
import Sources.Services
import Sources.Types exposing (Source)
import Tracks.Types exposing (IdentifiedTrack, Track)
import Tracks.Utils
import Types as TopLevel exposing (Illumination)
import Utils


-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.QueueMsg


makeEngineItem : Date -> List Source -> IdentifiedTrack -> EngineItem
makeEngineItem timestamp sources ( _, track ) =
    { track = track
    , url =
        sources
            |> List.find (findSource track.sourceId)
            |> Maybe.map (makeTrackUrl timestamp track)
            |> Maybe.withDefault "<missing-source>"
    }


makeItem : Bool -> IdentifiedTrack -> Item
makeItem isManualEntry identifiedTrack =
    { manualEntry = isManualEntry
    , identifiedTrack = identifiedTrack
    }



-- Private


findSource : String -> Source -> Bool
findSource wantedSourceId source =
    source.id == wantedSourceId


makeTrackUrl : Date -> Track -> Source -> String
makeTrackUrl timestamp track source =
    Sources.Services.makeTrackUrl
        source.service
        timestamp
        source.data
        Get
        track.path
