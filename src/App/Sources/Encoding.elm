module Sources.Encoding exposing (..)

{-| Encoding.
-}

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Sources.Types exposing (..)


-- Services

import Sources.Services.AmazonS3 as AmazonS3


-- Encode


encode : Source -> Encode.Value
encode source =
    Encode.object
        [ ( "id", Encode.string source.id )
        , ( "data", encodeData source.data )
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
    Decode.map3 Source
        (Decode.field "id" Decode.string)
        (Decode.field "data" (Decode.dict Decode.string))
        (Decode.field "service" serviceDecoder)


serviceDecoder : Decode.Decoder Service
serviceDecoder =
    Decode.map serviceStringToType Decode.string


serviceStringToType : String -> Service
serviceStringToType str =
    case str of
        "AmazonS3" ->
            AmazonS3

        _ ->
            AmazonS3
