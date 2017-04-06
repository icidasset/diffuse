module Tracks.Encoding exposing (..)

import Json.Decode exposing (Decoder, field, int, maybe, string)
import Json.Encode
import Tracks.Types exposing (..)


decode : Json.Encode.Value -> Track
decode value =
    {- TODO: `Result.withDefault` is probably a bad idea -}
    Json.Decode.decodeValue decoder value
        |> Result.withDefault emptyTrack


decoder : Decoder Track
decoder =
    Json.Decode.map3 Track
        (field "path" string)
        (field "sourceId" string)
        (field "tags" tagsDecoder)


tagsDecoder : Decoder Tags
tagsDecoder =
    Json.Decode.map6 Tags
        (maybe <| field "album" string)
        (maybe <| field "artist" string)
        (maybe <| field "genre" string)
        (maybe <| field "title" string)
        (maybe <| field "track" int)
        (maybe <| field "year" int)
