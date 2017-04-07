module Tracks.Utils exposing (..)

import Firebase.Data
import Tracks.Encoding
import Tracks.Types exposing (..)
import Types as TopLevel


-- 💧


decodeTracks : TopLevel.ProgramFlags -> List Track
decodeTracks flags =
    List.map Tracks.Encoding.decode (Maybe.withDefault [] flags.tracks)



-- 🔥


storeTracks : List Track -> Cmd TopLevel.Msg
storeTracks =
    List.map Tracks.Encoding.encode >> Firebase.Data.storeTracks
