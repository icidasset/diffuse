module Sources.Encoding exposing (..)

{-| Encoding.
-}

import Json.Encode
import Sources.Types exposing (..)


-- Services

import Sources.Services.AmazonS3 as AmazonS3


-- Functions


encode : SourceData -> Json.Encode.Value
encode data =
    case data of
        AmazonS3 s3Data ->
            AmazonS3.encode s3Data


decode : String -> Json.Encode.Value -> SourceData
decode typ value =
    case typ of
        "AmazonS3" ->
            AmazonS3 (AmazonS3.decode value)

        _ ->
            Debug.crash "TODO: Invalid source type"


dataType : SourceData -> String
dataType data =
    case data of
        AmazonS3 _ ->
            "AmazonS3"
