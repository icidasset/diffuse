module Sources.Utils exposing (..)

import Date
import Maybe.Extra as Maybe
import Regex
import Sources.Encoding
import Sources.Types exposing (..)
import Time
import Types as TopLevel exposing (Illumination)
import Users.Data
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
    List.map Sources.Encoding.encode >> Users.Data.storeSources



-- 🌱


isMusicFile : String -> Bool
isMusicFile =
    Regex.contains (Regex.regex "\\.(mp3|mp4|m4a)$")


selectMusicFiles : List String -> List String
selectMusicFiles =
    List.filter isMusicFile
