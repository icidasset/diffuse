module Tracks.Encoding exposing (decodeFavourite, decodeTrack, encodeFavourite, encodeGrouping, encodeMaybe, encodeSortBy, encodeSortDirection, encodeTags, encodeTrack, favouriteDecoder, groupingDecoder, sortByDecoder, sortDirectionDecoder, tagsDecoder, trackDecoder)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Time.Ext as Time
import Tracks exposing (..)



-- ENCODE


encodeFavourite : Favourite -> Encode.Value
encodeFavourite fav =
    Encode.object
        [ ( "artist", Encode.string fav.artist )
        , ( "title", Encode.string fav.title )
        ]


encodeGrouping : Grouping -> Encode.Value
encodeGrouping v =
    case v of
        AddedOn ->
            Encode.string "ADDED_ON"

        Directory ->
            Encode.string "DIRECTORY"

        FirstAlphaCharacter ->
            Encode.string "FIRST_ALPHA_CHARACTER"

        TrackYear ->
            Encode.string "TRACK_YEAR"


encodeSortBy : SortBy -> Encode.Value
encodeSortBy v =
    case v of
        Artist ->
            Encode.string "ARTIST"

        Album ->
            Encode.string "ALBUM"

        PlaylistIndex ->
            Encode.string "PLAYLIST_INDEX"

        Title ->
            Encode.string "TITLE"


encodeSortDirection : SortDirection -> Encode.Value
encodeSortDirection v =
    case v of
        Asc ->
            Encode.string "ASC"

        Desc ->
            Encode.string "DESC"


encodeTrack : Track -> Encode.Value
encodeTrack track =
    Encode.object
        [ ( "id", Encode.string track.id )
        , ( "insertedAt", Time.encode track.insertedAt )
        , ( "path", Encode.string track.path )
        , ( "sourceId", Encode.string track.sourceId )
        , ( "tags", encodeTags track.tags )
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


decodeFavourite : Decode.Value -> Maybe Favourite
decodeFavourite =
    Decode.decodeValue favouriteDecoder
        >> Result.toMaybe


decodeTrack : Decode.Value -> Maybe Track
decodeTrack =
    Decode.decodeValue trackDecoder
        >> Result.toMaybe


favouriteDecoder : Decode.Decoder Favourite
favouriteDecoder =
    Decode.map2 Favourite
        (Decode.field "artist" Decode.string)
        (Decode.field "title" Decode.string)


groupingDecoder : Decode.Decoder Grouping
groupingDecoder =
    Decode.andThen
        (\string ->
            case string of
                "ADDED_ON" ->
                    Decode.succeed AddedOn

                "DIRECTORY" ->
                    Decode.succeed Directory

                "FIRST_ALPHA_CHARACTER" ->
                    Decode.succeed FirstAlphaCharacter

                "TRACK_YEAR" ->
                    Decode.succeed TrackYear

                _ ->
                    Decode.fail "Invalid Grouping"
        )
        Decode.string


sortByDecoder : Decode.Decoder SortBy
sortByDecoder =
    Decode.andThen
        (\string ->
            case string of
                "ARTIST" ->
                    Decode.succeed Artist

                "ALBUM" ->
                    Decode.succeed Album

                "PLAYLIST_INDEX" ->
                    Decode.succeed PlaylistIndex

                "TITLE" ->
                    Decode.succeed Title

                _ ->
                    Decode.fail "Invalid SortBy"
        )
        Decode.string


sortDirectionDecoder : Decode.Decoder SortDirection
sortDirectionDecoder =
    Decode.andThen
        (\string ->
            case string of
                "ASC" ->
                    Decode.succeed Asc

                "DESC" ->
                    Decode.succeed Desc

                _ ->
                    Decode.fail "Invalid SortDirection"
        )
        Decode.string


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


trackDecoder : Decode.Decoder Track
trackDecoder =
    Decode.succeed Track
        |> required "id" Decode.string
        |> optional "insertedAt" Time.decoder Time.default
        |> required "path" Decode.string
        |> required "sourceId" Decode.string
        |> required "tags" tagsDecoder
