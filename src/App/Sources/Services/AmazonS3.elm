module Sources.Services.AmazonS3 exposing (..)

{-| Amazon S3 Service.

Resources:
- http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html

-}

import Debug
import Date exposing (Date)
import Http
import Sources.Services.AmazonS3.Parser as Parser
import Sources.Services.AmazonS3.Presign exposing (..)
import Sources.Services.AmazonS3.Types exposing (..)
import Sources.Types exposing (..)
import Time


-- Properties
-- 📟


{-| The list of properties we need from the user.
    `(property, label, placeholder)`
    Will be used for the forms.
-}
properties : List ( String, String, String )
properties =
    [ ( "accessKey", "Access key", "Fv6EWfLfCcMo" )
    , ( "secretKey", "Secret key", "qeNcqiMpgqC8" )
    , ( "bucketName", "Bucket name", "music" )
    , ( "region", "Region", initialProperties.region )
    , ( "directoryPath", "Directory", "/" )
    ]


{-| Initial properties.
-}
initialProperties : AmazonS3Source
initialProperties =
    { accessKey = "AKIAJZBG7YVSKEGIIUGA"
    , bucketName = "ongaku-ryoho-test"
    , directoryPath = "/"
    , name = "Amazon S3 source"
    , region = "eu-west-1"
    , secretKey = "eoUN4zF5oA3ajBfc2n1o61345j9m9dASgiVTKQ5j"
    }


{-| Translations.
    1. Given a property key, what is the actual value?
    2. Given a property key and a value, give me a new source.
-}
translateFrom : AmazonS3Source -> String -> String
translateFrom source propertyKey =
    case propertyKey of
        "accessKey" ->
            source.accessKey

        "bucketName" ->
            source.bucketName

        "directoryPath" ->
            source.directoryPath

        "region" ->
            source.region

        "secretKey" ->
            source.secretKey

        _ ->
            ""


translateTo : AmazonS3Source -> String -> String -> AmazonS3Source
translateTo source propertyKey propertyValue =
    case propertyKey of
        "accessKey" ->
            { source | accessKey = propertyValue }

        "bucketName" ->
            { source | bucketName = propertyValue }

        "directoryPath" ->
            { source | directoryPath = propertyValue }

        "region" ->
            { source | region = propertyValue }

        "secretKey" ->
            { source | secretKey = propertyValue }

        _ ->
            source



-- Track URL


{-| Create a public url for a file.
    We need this to play the track.
    (!) Creates a presigned url that's valid for 24 hours
-}
makeTrackUrl : Date -> AmazonS3Source -> String -> String
makeTrackUrl currentDate aws pathToFile =
    presignedUrl (Time.second * 86400) [] currentDate aws pathToFile



-- Tree


{-| Create a directory tree.
    List all the tracks in the bucket.
    Or a specific directory in the bucket.
-}
makeTree : (StepResult -> msg) -> AmazonS3Source -> Marker -> Date -> Cmd msg
makeTree msg aws marker currentDate =
    let
        marker_ =
            case marker of
                InProgress s ->
                    s

                _ ->
                    ""

        params =
            []

        url =
            presignedUrl (Time.second * 60) params currentDate aws aws.directoryPath
    in
        url
            |> Debug.log "url"
            |> Http.getString
            |> Http.send msg


handleTreeResponse : ProcessingContext -> String -> ProcessingContext
handleTreeResponse context response =
    let
        parsedResponse =
            Parser.parseResponse response
    in
        { context
            | filePaths = context.filePaths ++ parsedResponse.filePaths
            , treeMarker = InProgress parsedResponse.marker
        }
