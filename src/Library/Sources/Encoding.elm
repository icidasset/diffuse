module Sources.Encoding exposing (decode, decoder, encode, encodeData, serviceDecoder)

{-| Encoding.
-}

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Sources exposing (..)
import Sources.Services as Services



-- ENCODE


encode : Source -> Encode.Value
encode source =
    Encode.object
        [ ( "id", Encode.string source.id )
        , ( "data", encodeData source.data )
        , ( "directoryPlaylists", Encode.bool source.directoryPlaylists )
        , ( "enabled", Encode.bool source.enabled )
        , ( "service", Encode.string (Services.typeToKey source.service) )
        ]


encodeData : SourceData -> Encode.Value
encodeData data =
    data
        |> Dict.toList
        |> List.map (Tuple.mapSecond Encode.string)
        |> Encode.object



-- DECODE


decode : Decode.Value -> Maybe Source
decode value =
    Decode.decodeValue decoder value
        |> Result.toMaybe


decoder : Decode.Decoder Source
decoder =
    Decode.map5 Source
        (Decode.field "id" Decode.string)
        (Decode.field "data" (Decode.dict Decode.string))
        (Decode.field "directoryPlaylists" Decode.bool
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault True)
        )
        (Decode.field "enabled" Decode.bool
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault True)
        )
        (Decode.field "service" serviceDecoder)


serviceDecoder : Decode.Decoder Service
serviceDecoder =
    Decode.andThen
        (\key ->
            case Services.keyToType key of
                Just service ->
                    Decode.succeed service

                Nothing ->
                    Decode.fail "Unrecognizable source service"
        )
        Decode.string
