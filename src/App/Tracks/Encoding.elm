module Tracks.Encoding exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Tracks.Types exposing (..)


-- Encode


encode : Track -> Encode.Value
encode track =
    Encode.object
        [ ( "path", Encode.string track.path )
        , ( "sourceId", Encode.string track.sourceId )
        , ( "tags", encodeTags track.tags )
        ]


encodeTags : Tags -> Encode.Value
encodeTags tags =
    Encode.object
        [ ( "album", encodeMaybe tags.album Encode.string )
        , ( "artist", encodeMaybe tags.artist Encode.string )
        , ( "genre", encodeMaybe tags.genre Encode.string )
        , ( "nr", encodeMaybe tags.nr Encode.int )
        , ( "title", encodeMaybe tags.title Encode.string )
        , ( "year", encodeMaybe tags.year Encode.int )
        ]


encodeMaybe : Maybe a -> (a -> Encode.Value) -> Encode.Value
encodeMaybe maybe encoder =
    maybe
        |> Maybe.map encoder
        |> Maybe.withDefault Encode.null



-- Decode


decode : Decode.Value -> Track
decode value =
    {- TODO: `Result.withDefault` is probably a bad idea -}
    Decode.decodeValue decoder value
        |> Result.withDefault emptyTrack


decoder : Decode.Decoder Track
decoder =
    Decode.map3 Track
        (Decode.field "path" Decode.string)
        (Decode.field "sourceId" Decode.string)
        (Decode.field "tags" tagsDecoder)


tagsDecoder : Decode.Decoder Tags
tagsDecoder =
    Decode.map6 Tags
        (Decode.maybe <| Decode.field "album" Decode.string)
        (Decode.maybe <| Decode.field "artist" Decode.string)
        (Decode.maybe <| Decode.field "genre" Decode.string)
        (Decode.maybe <| Decode.field "nr" Decode.int)
        (Decode.maybe <| Decode.field "title" Decode.string)
        (Decode.maybe <| Decode.field "year" Decode.int)
