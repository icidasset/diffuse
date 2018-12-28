module Tracks.Encoding exposing (decodeFavourite, decodeTrack, encodeFavourite, encodeMaybe, encodeTags, encodeTrack, favouriteDecoder, tagsDecoder, trackDecoder)

import Json.Decode as Decode
import Json.Encode as Encode
import Tracks exposing (..)



-- ENCODE


encodeTrack : Track -> Encode.Value
encodeTrack track =
    Encode.object
        [ ( "id", Encode.string track.id )
        , ( "path", Encode.string track.path )
        , ( "sourceId", Encode.string track.sourceId )
        , ( "tags", encodeTags track.tags )
        ]


encodeFavourite : Favourite -> Encode.Value
encodeFavourite fav =
    Encode.object
        [ ( "artist", Encode.string fav.artist )
        , ( "title", Encode.string fav.title )
        ]


encodeTags : Tags -> Encode.Value
encodeTags tags =
    Encode.object
        [ ( "disc", Encode.int tags.disc )
        , ( "nr", Encode.int tags.nr )

        --
        , ( "album", Encode.string tags.album )
        , ( "artist", Encode.string tags.artist )
        , ( "title", Encode.string tags.title )

        --
        , ( "genre", encodeMaybe tags.genre Encode.string )
        , ( "picture", encodeMaybe tags.picture Encode.string )
        , ( "year", encodeMaybe tags.year Encode.int )
        ]


encodeMaybe : Maybe a -> (a -> Encode.Value) -> Encode.Value
encodeMaybe maybe encoder =
    maybe
        |> Maybe.map encoder
        |> Maybe.withDefault Encode.null



-- DECODE


decodeTrack : Decode.Value -> Maybe Track
decodeTrack =
    Decode.decodeValue trackDecoder
        >> Result.toMaybe


decodeFavourite : Decode.Value -> Maybe Favourite
decodeFavourite =
    Decode.decodeValue favouriteDecoder
        >> Result.toMaybe


trackDecoder : Decode.Decoder Track
trackDecoder =
    Decode.map4 Track
        (Decode.field "id" Decode.string)
        (Decode.field "path" Decode.string)
        (Decode.field "sourceId" Decode.string)
        (Decode.field "tags" tagsDecoder)


tagsDecoder : Decode.Decoder Tags
tagsDecoder =
    Decode.map8 Tags
        (Decode.field "disc" Decode.int)
        (Decode.field "nr" Decode.int)
        (Decode.field "album" Decode.string)
        (Decode.field "artist" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.maybe <| Decode.field "genre" Decode.string)
        (Decode.maybe <| Decode.field "picture" Decode.string)
        (Decode.maybe <| Decode.field "year" Decode.int)


favouriteDecoder : Decode.Decoder Favourite
favouriteDecoder =
    Decode.map2 Favourite
        (Decode.field "artist" Decode.string)
        (Decode.field "title" Decode.string)
