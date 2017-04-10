module Queue.Utils exposing (($), makeQueueItem)

import Date exposing (Date)
import List.Extra as List
import Queue.Types as Queue exposing (..)
import Sources.Processing
import Sources.Types exposing (Source)
import Tracks.Types exposing (Track)
import Types as TopLevel exposing (Illumination)
import Utils


-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.QueueMsg



-- 🌱


makeQueueItem : Bool -> Date -> List Source -> Track -> Queue.Item
makeQueueItem isManualEntry timestamp sources track =
    { id = track.sourceId ++ "-" ++ track.path
    , manualEntry = isManualEntry
    , track = track
    , url =
        sources
            |> List.find (findSource track.sourceId)
            |> Maybe.map (makeTrackUrl timestamp track)
            |> Maybe.withDefault "<missing-source>"
    }



-- Private


findSource : String -> Source -> Bool
findSource wantedSourceId source =
    source.id == wantedSourceId


makeTrackUrl : Date -> Track -> Source -> String
makeTrackUrl timestamp track source =
    Sources.Processing.makeTrackUrl timestamp source track.path
