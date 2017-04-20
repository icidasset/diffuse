module Tracks.Utils exposing (..)

import Firebase.Data
import Maybe.Extra as Maybe
import Tracks.Encoding
import Tracks.Ports as Ports
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


partial : Int
partial =
    50



-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.TracksMsg


handleNewCollection : List Track -> Cmd TopLevel.Msg
handleNewCollection tracks =
    let
        encodedTracks =
            List.map Tracks.Encoding.encode tracks
    in
        Cmd.batch
            [ Firebase.Data.storeTracks encodedTracks
            , Ports.updateSearchIndex encodedTracks -- TODO: Perform search?
            ]
