module Sources.Services.AmazonS3.Presign exposing (presignedUrl)

import Binary exposing (Bits)
import Crypto.HMAC as HMAC
import DateFormat as Date
import Dict
import Dict.Ext as Dict
import Hex
import Maybe.Extra as Maybe
import Regex
import SHA
import Sources exposing (SourceData)
import Sources.Processing exposing (HttpMethod, httpMethod)
import String.Ext as String
import Time
import Url
import Url.Builder as Url



-- 🔱


presignedUrl :
    HttpMethod
    -> Int
    -> List ( String, String )
    -> Time.Posix
    -> SourceData
    -> String
    -> String
presignedUrl method lifeExpectancyInSeconds extraParams currentTime srcData pathToFile =
    let
        aws =
            srcData

        region =
            Dict.fetchUnknown "region" aws

        bucketName =
            Dict.fetchUnknown "bucketName" aws

        customHost =
            case Dict.fetch "host" "" aws of
                "" ->
                    Nothing

                x ->
                    Just x

        host =
            case customHost of
                Just h ->
                    h
                        |> String.chopStart "http://"
                        |> String.chopStart "https://"
                        |> String.chopEnd "/"

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
            pathToFile
                |> String.chopStart "/"
                |> String.split "/"
                |> List.map (Url.percentEncode >> encodeAdditionalCharacters)
                |> String.join "/"
                |> String.append ("/" ++ filePathPrefix)

        -- {var} Time
        -- timestamp    -> 20130721T201207Z
        -- date         -> 20130721
        timestamp =
            Date.format
                [ Date.yearNumber
                , Date.monthFixed
                , Date.dayOfMonthFixed
                , Date.text "T"
                , Date.hourMilitaryFixed
                , Date.minuteFixed
                , Date.secondFixed
                , Date.text "Z"
                ]
                Time.utc
                currentTime

        date =
            Date.format
                [ Date.yearNumber
                , Date.monthFixed
                , Date.dayOfMonthFixed
                ]
                Time.utc
                currentTime

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
            , ( "X-Amz-Expires", String.fromInt lifeExpectancyInSeconds )
            , ( "X-Amz-SignedHeaders", "host" )
            ]
                |> List.append extraParams
                |> List.sortBy Tuple.first
                |> List.map (\( a, b ) -> Url.string a b)
                |> Url.toQuery
                |> String.dropLeft 1
                |> encodeAdditionalCharacters

        request =
            String.join
                "\n"
                [ httpMethod method
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

                --
                , request
                    |> Binary.fromStringAsUtf8
                    |> SHA.sha256
                    |> Binary.toHex
                    |> String.toLower
                ]

        -- Signature
        signature =
            ("AWS4" ++ Dict.fetchUnknown "secretKey" aws)
                |> Binary.fromStringAsUtf8
                |> hmacSha256 date
                |> hmacSha256 region
                |> hmacSha256 "s3"
                |> hmacSha256 "aws4_request"
                |> hmacSha256 stringToSign
                |> Binary.toHex
                |> String.toLower
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



-- ⚗️


encodeAdditionalCharacters : String -> String
encodeAdditionalCharacters query =
    Regex.replace
        (Maybe.withDefault Regex.never <| Regex.fromString "[!*'()]")
        (\{ match } ->
            match
                |> String.toList
                |> List.map (Char.toCode >> Hex.toString >> String.toUpper >> (++) "%")
                |> String.concat
        )
        query


hmacSha256 : String -> Bits -> Bits
hmacSha256 =
    HMAC.encrypt64 SHA.sha256
