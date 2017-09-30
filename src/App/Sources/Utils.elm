module Sources.Utils exposing (..)

import Date
import Maybe.Extra as Maybe
import Response.Ext exposing (do)
import Sources.Types exposing (..)
import Tracks.Types
import Time
import Types as TopLevel exposing (Illumination)
import Utils


-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.SourcesMsg


pickEnableSourceIds : List Source -> List SourceId
pickEnableSourceIds collection =
    collection
        |> List.filter (.enabled >> (==) True)
        |> List.map .id


setProperSourceId : Model -> Source -> Source
setProperSourceId model source =
    { source
        | id =
            model.timestamp
                |> Date.toTime
                |> Time.inMilliseconds
                |> round
                |> toString
                |> flip String.append (List.length model.collection |> (+) 1 |> toString)
    }


updateEnabledSourceIds : List Source -> Cmd TopLevel.Msg
updateEnabledSourceIds collection =
    collection
        |> pickEnableSourceIds
        |> Tracks.Types.SetEnabledSourceIds
        |> TopLevel.TracksMsg
        |> do


sourcesHaveUpdated : List Source -> Cmd TopLevel.Msg
sourcesHaveUpdated updatedCollection =
    Cmd.batch
        [ updateEnabledSourceIds updatedCollection
        , do TopLevel.StoreUserData
        ]
