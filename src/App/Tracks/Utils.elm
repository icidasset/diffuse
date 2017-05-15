module Tracks.Utils exposing (..)

import Json.Encode as Json
import Maybe.Extra as Maybe
import Tracks.Encoding
import Tracks.Types exposing (..)
import Types as TopLevel exposing (Illumination)
import Utils


-- 💧


decodeTracks : List Json.Value -> List Track
decodeTracks encodedTracks =
    encodedTracks
        |> List.map Tracks.Encoding.decodeTrack
        |> Maybe.values


decodeFavourites : List Json.Value -> List Favourite
decodeFavourites encodedFavourites =
    encodedFavourites
        |> List.map Tracks.Encoding.decodeFavourite
        |> Maybe.values


partial : Int
partial =
    50



-- 🔥


($) : Illumination Model Msg
($) =
    Utils.illuminate TopLevel.TracksMsg



-- 🌱


getIdentifiers : IdentifiedTrack -> Identifiers
getIdentifiers =
    Tuple.first


unindentify : IdentifiedTrack -> Track
unindentify =
    Tuple.second
