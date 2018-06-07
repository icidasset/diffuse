module Sources.Utils exposing (..)

import Date
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



-- Viability


isViable : ViabilityDependencies -> Source -> Bool
isViable deps source =
    case source.service of
        Sources.Types.Local ->
            deps.isElectron

        Sources.Types.WebDav ->
            deps.isElectron

        _ ->
            deps.isOnline


{-| Some types of sources are only usable on certain platforms.
Therefor in some situations we need to filter out the unusable ones.
-}
viableSourcesOnly : TopLevel.Model -> List Source -> List Source
viableSourcesOnly model =
    { isElectron = model.isElectron
    , isOnline = model.isOnline
    }
        |> isViable
        |> List.filter
