module Tracks.Encoding exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Tracks.Types exposing (..)


-- Encode


encode : Track -> Encode.Value
encode track =
    Encode.object
        [ ( "id", Encode.string track.id )
        , ( "path", Encode.string track.path )
        , ( "sourceId", Encode.string track.sourceId )
        , ( "tags", encodeTags track.tags )
        ]


encodeTags : Tags -> Encode.Value
encodeTags tags =
    Encode.object
        [ ( "nr", Encode.int tags.nr )

        --
        , ( "album", Encode.string tags.album )
        , ( "artist", Encode.string tags.artist )
        , ( "title", Encode.string tags.title )

        --
        , ( "genre", encodeMaybe tags.genre Encode.string )
        , ( "year", encodeMaybe tags.year Encode.int )
        ]


encodeMaybe : Maybe a -> (a -> Encode.Value) -> Encode.Value
encodeMaybe maybe encoder =
    maybe
        |> Maybe.map encoder
        |> Maybe.withDefault Encode.null



-- Decode


decode : Decode.Value -> Maybe Track
decode value =
    Decode.decodeValue decoder value
        |> Result.toMaybe


decoder : Decode.Decoder Track
decoder =
    Decode.map4 Track
        (Decode.field "id" Decode.string)
        (Decode.field "path" Decode.string)
        (Decode.field "sourceId" Decode.string)
        (Decode.field "tags" tagsDecoder)


tagsDecoder : Decode.Decoder Tags
tagsDecoder =
    Decode.map6 Tags
        (Decode.field "nr" Decode.int)
        (Decode.field "album" Decode.string)
        (Decode.field "artist" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.maybe <| Decode.field "genre" Decode.string)
        (Decode.maybe <| Decode.field "year" Decode.int)
