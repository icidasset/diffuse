module Sources.Services.Azure.Authorization exposing (Computation(..), SignatureDependencies, StorageMethod(..), makeSignature, presignedUrl)

{-| Resources:

  - <https://docs.microsoft.com/en-us/rest/api/storageservices/constructing-an-account-sas#account-sas-example>
  - <https://docs.microsoft.com/en-us/rest/api/storageservices/Constructing-a-Service-SAS?redirectedfrom=MSDN>

-}

import Binary
import BinaryBase64
import Common
import Cryptography.HMAC as Hmac
import DateFormat as Date
import Dict
import Dict.Ext as Dict
import SHA
import Sources exposing (SourceData)
import Sources.Processing exposing (HttpMethod)
import String.Ext as String
import Time
import Url



-- Types


type Computation
    = List
    | Read


type StorageMethod
    = Blob
    | File



-- Public functions


presignedUrl :
    StorageMethod
    -> Computation
    -> HttpMethod
    -> Int
    -> Time.Posix
    -> SourceData
    -> String
    -> List ( String, String )
    -> String
presignedUrl storageMethod computation httpMethod hoursToLive currentTime srcData pathToFile params =
    let
        azure =
            srcData

        accountName =
            Dict.fetchUnknown "accountName" azure

        accountKey =
            Dict.fetchUnknown "accountKey" azure

        container =
            Dict.fetchUnknown "container" azure

        -- {var} Time (y-MM-ddTHH:mmZ)
        expiryTime =
            Date.format
                [ Date.yearNumber
                , Date.text "-"
                , Date.monthFixed
                , Date.text "-"
                , Date.dayOfMonthFixed
                , Date.text "T"
                , Date.hourMilitaryFixed
                , Date.text ":"
                , Date.minuteFixed
                , Date.text "Z"
                ]
                Time.utc
                (currentTime
                    |> Time.posixToMillis
                    |> (+) 3600000
                    |> Time.millisToPosix
                )

        -- {var} Other
        permissions =
            case computation of
                List ->
                    "l"

                Read ->
                    "r"

        resourceType =
            case storageMethod of
                Blob ->
                    "blob"

                File ->
                    "file"

        resType =
            case storageMethod of
                Blob ->
                    "container"

                File ->
                    "directory"

        -- Signature
        signatureStuff =
            { accountKey = accountKey
            , accountName = accountName
            , expiryTime = expiryTime
            , permissions = permissions
            , protocol = "https"
            , resources = "co"
            , services = "bf"
            , startTime = ""
            , version = "2017-04-17"
            }
    in
    String.concat
        [ "https://"
        , Url.percentEncode accountName
        , "."
        , Url.percentEncode resourceType
        , ".core.windows.net/"
        , Url.percentEncode container
        , "/"
        , Url.percentEncode (String.chopStart "/" pathToFile)

        -- Start query params
        , case Common.queryString params of
            "" ->
                "?"

            qs ->
                qs

        -- Query params for certain requests
        , case computation of
            List ->
                "&restype=" ++ resType ++ "&comp=list"

            _ ->
                ""

        -- Signature things
        , "&sv="
        , Url.percentEncode signatureStuff.version
        , "&ss="
        , Url.percentEncode signatureStuff.services
        , "&srt="
        , Url.percentEncode signatureStuff.resources
        , "&sp="
        , Url.percentEncode signatureStuff.permissions
        , "&se="
        , Url.percentEncode signatureStuff.expiryTime
        , "&spr="
        , Url.percentEncode signatureStuff.protocol
        , "&sig="
        , Url.percentEncode (makeSignature signatureStuff)
        ]



-- Signature


type alias SignatureDependencies =
    { accountKey : String
    , accountName : String
    , expiryTime : String
    , permissions : String
    , protocol : String
    , resources : String
    , services : String
    , startTime : String
    , version : String
    }


{-| Make a signature.

    >>> makeSignature { accountKey = "93K17Co74T2lDHk2rA+wmb/avIAS6u6lPnZrk2hyT+9+aov82qNhrcXSNGZCzm9mjd4d75/oxxOr6r1JVpgTLA==", accountName = "tsmatsuzsttest0001", expiryTime = "2016-07-08T04:41:20Z", permissions = "rwdlacup", protocol = "https", resources = "sco", services = "bfqt", startTime = "2016-06-29T04:41:20Z", version = "2015-04-05" }
    "+XuDjuLE1Sv/FrJTLz8YjsaDukWNTKX7e8G8Ew+5aps="

-}
makeSignature : SignatureDependencies -> String
makeSignature { accountKey, accountName, expiryTime, permissions, protocol, resources, services, startTime, version } =
    let
        message =
            -- accountname + "\n" +
            -- signedpermissions + "\n" +
            -- signedservice + "\n" +
            -- signedresourcetype + "\n" +
            -- signedstart + "\n" +
            -- signedexpiry + "\n" +
            -- signedIP + "\n" +
            -- signedProtocol + "\n" +
            -- signedversion + "\n"
            String.join "\n"
                [ accountName
                , permissions
                , services
                , resources
                , startTime
                , expiryTime
                , ""
                , protocol
                , version ++ "\n"
                ]
    in
    accountKey
        |> BinaryBase64.decode
        |> Result.withDefault []
        |> List.map (Binary.fromDecimal >> Binary.ensureSize 8)
        |> Binary.concat
        |> Hmac.encrypt64 SHA.sha256 message
        |> Binary.chunksOf 8
        |> List.map Binary.toDecimal
        |> BinaryBase64.encode
