module Sources.Services.AmazonS3.Presign exposing (presignedUrl)

import Date exposing (Date)
import Date.Extra
import Debug
import Http
import SHA
import Sources.Crypto.Hex exposing (..)
import Sources.Crypto.Hmac as Hmac
import Sources.Services.AmazonS3.Types exposing (..)
import Time exposing (Time)
import Utils


presignedUrl : Time -> List ( String, String ) -> Date -> AmazonS3Source -> String -> String
presignedUrl lifeExpectancy extraParams currentDate dirtyAws pathToFile =
    let
        aws =
            { dirtyAws
                | accessKey = String.trim dirtyAws.accessKey
                , bucketName = String.trim dirtyAws.bucketName
                , region = String.trim dirtyAws.region
                , secretKey = String.trim dirtyAws.secretKey
            }

        host =
            aws.bucketName ++ ".s3.amazonaws.com"

        filePath =
            if String.startsWith "/" pathToFile then
                pathToFile
            else
                "/" ++ pathToFile

        timestamp =
            Date.Extra.toUtcFormattedString "yMMddTHHmmssZ" currentDate

        date =
            Date.Extra.toUtcFormattedString "yMMdd" currentDate

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
            , ( "X-Amz-Date", timestamp )
            , ( "X-Amz-Expires", toString lifeExpectancyInSeconds )
            , ( "X-Amz-SignedHeaders", "host" )
            ]
                |> List.append extraParams
                |> List.sortBy Tuple.first
                |> List.map Utils.makeQueryParam
                |> String.join "&"

        request =
            String.join
                "\n"
                [ "GET"
                , filePath
                , queryString
                , "host:" ++ host
                , ""
                , "host"
                , "UNSIGNED-PAYLOAD"
                ]

        -- String to sign
        stringToSign =
            String.join
                "\n"
                [ "AWS4-HMAC-SHA256"
                , timestamp
                , String.join "/" [ date, aws.region, "s3", "aws4_request" ]
                , SHA.sha256sum request
                ]

        -- Signature
        signature =
            ("AWS4" ++ aws.secretKey)
                |> Hmac.encrypt64 SHA.sha256sum date
                |> Hmac.encrypt64 SHA.sha256sum aws.region
                |> Hmac.encrypt64 SHA.sha256sum "s3"
                |> Hmac.encrypt64 SHA.sha256sum "aws4_request"
                |> Hmac.encrypt64 SHA.sha256sum stringToSign
                |> unicodeToHex 2
    in
        String.concat
            [ "https://"
            , host
            , filePath
            , "?"
            , queryString
            , "&X-Amz-Signature="
            , signature
            ]
