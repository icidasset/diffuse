module Sources.Services.Aws exposing (makeTrackUrl)

{-| AWS Service.

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


-- Hooks


makeTrackUrl : Date -> AwsSource -> String -> String
makeTrackUrl currentDate aws pathToFile =
    -- Create a presigned url that's valid for 24 hours
    presignedUrl 86400 [] currentDate aws pathToFile


makeTree : Date -> AwsSource -> List String
makeTree currentDate aws =
    -- TODO
    -- presignedUrl 60 [ ( "marker", marker ), ( "max-keys", "1000" ) ] currentDate aws "/"
    []



-- Signature


presignedUrl : Time -> List ( String, String ) -> Date -> AwsSource -> String -> String
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
