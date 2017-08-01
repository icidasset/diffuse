module Sources.Utils exposing (..)

import Date
import Maybe.Extra as Maybe
import Response.Ext exposing (do)
import Sources.Encoding
import Sources.Types exposing (..)
import Tracks.Types
import Time
import Types as TopLevel exposing (Illumination)
import Users.Ports
import Utils


-- 💧


decodeSources : TopLevel.ProgramFlags -> List Source
decodeSources flags =
    flags.sources
        |> Maybe.withDefault []
        |> List.map Sources.Encoding.decode
        |> Maybe.values



-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.SourcesMsg


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


storeSources : List Source -> Cmd TopLevel.Msg
storeSources =
    List.map Sources.Encoding.encode >> Users.Ports.storeSources


updateEnabledSourceIds : List Source -> Cmd TopLevel.Msg
updateEnabledSourceIds collection =
    collection
        |> List.filter (.enabled >> (==) True)
        |> List.map .id
        |> Tracks.Types.SetEnabledSourceIds
        |> TopLevel.TracksMsg
        |> do
