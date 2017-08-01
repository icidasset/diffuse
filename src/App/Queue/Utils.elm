module Queue.Utils exposing (($), makeItem, makeEngineItem)

import Date exposing (Date)
import List.Extra as List
import Queue.Types exposing (..)
import Sources.Processing
import Sources.Services
import Sources.Types exposing (HttpMethod(..), Source)
import Tracks.Types exposing (Track)
import Types as TopLevel exposing (Illumination)
import Utils


-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.QueueMsg


makeEngineItem : Date -> List Source -> Track -> EngineItem
makeEngineItem timestamp sources track =
    { track = track
    , url =
        sources
            |> List.find (findSource track.sourceId)
            |> Maybe.map (makeTrackUrl timestamp track)
            |> Maybe.withDefault "<missing-source>"
    }


makeItem : Bool -> Track -> Item
makeItem isManualEntry track =
    { manualEntry = isManualEntry
    , track = track
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
