module Sources.Services.AmazonS3
    exposing
        ( properties
        , initialProperties
        , translateFrom
        , translateTo
        , makeTrackUrl
        )

{-| Amazon S3 Service.

Resources:
- http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html

-}

import Date exposing (Date)
import Date.Format
import Http
import SHA
import Sources.Crypto.Hmac as Hmac
import Sources.Types exposing (..)
import String.Interpolate exposing (interpolate)
import Time exposing (Time)
import Utils


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
    { accessKey = ""
    , bucketName = ""
    , directoryPath = "/"
    , name = "Amazon S3 source"
    , region = "us-west-2"
    , secretKey = ""
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


{-| Create a public url for a file.
    We need this to play the track.
-}
makeTrackUrl : Date -> AmazonS3Source -> String -> String
makeTrackUrl currentDate aws pathToFile =
    -- Create a presigned url that's valid for 24 hours
    presignedUrl 86400 [] currentDate aws pathToFile


{-| Create a directory tree.
    List all the tracks in the bucket.
    Or a specific directory in the bucket.
-}
makeTree : Date -> AmazonS3Source -> List String
makeTree currentDate aws =
    -- TODO
    -- presignedUrl 60 [ ( "marker", marker ), ( "max-keys", "1000" ) ] currentDate aws "/"
    []



-- 🚫


presignedUrl : Time -> List ( String, String ) -> Date -> AmazonS3Source -> String -> String
presignedUrl lifeExpectancy extraParams currentDate aws pathToFile =
    let
        host =
            aws.bucketName ++ ".s3.amazonaws.com"

        filePath =
            if String.startsWith "/" pathToFile then
                pathToFile
            else
                "/" ++ pathToFile

        timestamp =
            Date.Format.formatISO8601 currentDate

        date =
            Date.Format.format "%Y%M%D" currentDate

        lifeExpectancyInSeconds =
            Time.inSeconds lifeExpectancy

        -- Request
        credential =
            [ aws.accessKey
            , date
            , aws.region
            , "s3"
            , "aws4_request"
            ]
                |> String.join "/"

        queryString =
            [ ( "X-Amz-Algorithm", "AWS4-HMAC-SHA256" )
            , ( "X-Amz-Credential", credential )
            , ( "X-Amz-Date", date )
            , ( "X-Amz-Expires", toString lifeExpectancyInSeconds )
            , ( "X-Amz-SignedHeaders", "host" )
            ]
                |> List.append extraParams
                |> List.map Utils.makeQueryParam
                |> String.join "&"
                |> String.append "?"

        request =
            String.join
                "\n"
                [ "GET"
                , Http.encodeUri filePath
                , Http.encodeUri queryString
                , "host:" ++ host
                , ""
                , "UNSIGNED-PAYLOAD"
                ]
                |> SHA.sha256sum
                |> Utils.lowercaseHexadecimalString

        -- String to sign
        stringToSign =
            String.join
                "\n"
                [ "AWS4-HMAC-SHA256"
                , timestamp
                , String.join "/" [ date, aws.region, "s3", "aws4_request" ]
                , request
                ]
    in
        ("AWS4" ++ aws.secretKey)
            |> Hmac.encrypt64 SHA.sha256sum date
            |> Hmac.encrypt64 SHA.sha256sum aws.region
            |> Hmac.encrypt64 SHA.sha256sum "s3"
            |> Hmac.encrypt64 SHA.sha256sum "aws4_request"
            |> Hmac.encrypt64 SHA.sha256sum stringToSign
            |> Utils.lowercaseHexadecimalString
