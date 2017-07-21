module Sources.Encoding exposing (..)

{-| Encoding.
-}

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Sources.Types exposing (..)


-- Encode


encode : Source -> Encode.Value
encode source =
    Encode.object
        [ ( "id", Encode.string source.id )
        , ( "data", encodeData source.data )
        , ( "enabled", Encode.bool source.enabled )
        , ( "service", Encode.string (toString source.service) )
        ]


encodeData : SourceData -> Encode.Value
encodeData data =
    data
        |> Dict.toList
        |> List.map (Tuple.mapSecond Encode.string)
        |> Encode.object



-- Decode


decode : Decode.Value -> Maybe Source
decode value =
    Decode.decodeValue decoder value
        |> Result.toMaybe


decoder : Decode.Decoder Source
decoder =
    Decode.map4 Source
        (Decode.field "id" Decode.string)
        (Decode.field "data" (Decode.dict Decode.string))
        (Decode.field "enabled" Decode.bool
            |> Decode.maybe
            |> Decode.map (Maybe.withDefault True)
        )
        (Decode.field "service" serviceDecoder)


serviceDecoder : Decode.Decoder Service
serviceDecoder =
    Decode.map serviceStringToType Decode.string


serviceStringToType : String -> Service
serviceStringToType str =
    case str of
        "AmazonS3" ->
            AmazonS3

        "Ipfs" ->
            Ipfs

        _ ->
            AmazonS3
