module Sources.Services.AmazonS3.Presign exposing (presignedUrl)

import Date exposing (Date)
import Date.Extra
import Dict
import Dict.Ext as Dict
import Maybe.Extra as Maybe
import SHA
import Sources.Crypto.Hex exposing (..)
import Sources.Crypto.Hmac as Hmac
import Sources.Processing.Types exposing (HttpMethod)
import Sources.Types exposing (SourceData)
import Sources.Services.Utils exposing (replace)
import Time exposing (Time)
import Utils


presignedUrl :
    HttpMethod
    -> Time
    -> List ( String, String )
    -> Date
    -> SourceData
    -> String
    -> String
presignedUrl method lifeExpectancy extraParams currentDate srcData pathToFile =
    let
        aws =
            srcData

        region =
            Dict.fetchUnknown "region" aws

        bucketName =
            Dict.fetchUnknown "bucketName" aws

        customHost =
            Dict.get "host" aws

        host =
            case customHost of
                Just h ->
                    h
                        |> replace "^http://" ""
                        |> replace "^https://" ""
                        |> replace "/$" ""

                Nothing ->
                    bucketName ++ ".s3.amazonaws.com"

        protocol =
            if String.contains "http://" (Maybe.withDefault "" customHost) then
                "http://"
            else
                "https://"

        -- {var} Paths
        filePathPrefix =
            if Maybe.isJust customHost then
                "/" ++ bucketName
            else
                ""

        filePath =
            (if String.startsWith "/" pathToFile then
                pathToFile
             else
                "/" ++ pathToFile
            )
                |> String.split "/"
                |> List.map Utils.encodeUri
                |> String.join "/"
                |> String.append filePathPrefix

        -- {var} Time
        timestamp =
            Date.Extra.toUtcFormattedString "yMMddTHHmmssZ" currentDate

        date =
            Date.Extra.toUtcFormattedString "yMMdd" currentDate

        lifeExpectancyInSeconds =
            Time.inSeconds lifeExpectancy

        -- Request
        credential =
            [ Dict.fetchUnknown "accessKey" aws
            , date
            , region
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
                [ method
                    |> toString
                    |> String.toUpper
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
                , String.join "/" [ date, region, "s3", "aws4_request" ]
                , SHA.sha256sum request
                ]

        -- Signature
        signature =
            ("AWS4" ++ Dict.fetchUnknown "secretKey" aws)
                |> Hmac.encrypt64 SHA.sha256sum date
                |> Hmac.encrypt64 SHA.sha256sum region
                |> Hmac.encrypt64 SHA.sha256sum "s3"
                |> Hmac.encrypt64 SHA.sha256sum "aws4_request"
                |> Hmac.encrypt64 SHA.sha256sum stringToSign
                |> unicodeToHex 2
    in
        String.concat
            [ protocol
            , host
            , filePath
            , "?"
            , queryString
            , "&X-Amz-Signature="
            , signature
            ]
