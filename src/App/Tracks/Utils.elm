module Tracks.Utils exposing (..)

import Firebase.Data
import Maybe.Extra as Maybe
import Tracks.Encoding
import Tracks.Types exposing (..)
import Types as TopLevel exposing (Illumination)
import Utils


-- 💧


decodeTracks : TopLevel.ProgramFlags -> List Track
decodeTracks flags =
    flags.tracks
        |> Maybe.withDefault []
        |> List.map Tracks.Encoding.decode
        |> Maybe.values



-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.TracksMsg


storeTracks : List Track -> Cmd TopLevel.Msg
storeTracks =
    List.map Tracks.Encoding.encode >> Firebase.Data.storeTracks
