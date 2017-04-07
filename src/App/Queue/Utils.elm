module Queue.Utils exposing (($), makeQueueItem)

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


makeQueueItem : TopLevel.Model -> Track -> Queue.Item
makeQueueItem model track =
    { id = track.sourceId ++ "-" ++ track.path
    , manualEntry = True
    , track = track
    , url =
        model.sources.collection
            |> List.find (findSource track.sourceId)
            |> Maybe.map (makeTrackUrl model track)
            |> Maybe.withDefault "<missing-source>"
    }



-- Private


findSource : String -> Source -> Bool
findSource wantedSourceId source =
    source.id == wantedSourceId


makeTrackUrl : TopLevel.Model -> Track -> Source -> String
makeTrackUrl model track source =
    Sources.Processing.makeTrackUrl model.timestamp source track.path
