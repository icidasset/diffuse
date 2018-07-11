module Sources.Services.Azure.Authorization exposing (..)

{-| Resources:

  - <https://docs.microsoft.com/en-us/rest/api/storageservices/constructing-an-account-sas#account-sas-example>
  - <https://docs.microsoft.com/en-us/rest/api/storageservices/Constructing-a-Service-SAS?redirectedfrom=MSDN>

-}

import Base64
import BinaryBase64
import Date exposing (Date)
import Date.Extra
import Dict
import Dict.Ext as Dict
import Crypto.Hash as SHA
import Sources.Crypto.Hex as Hex
import Sources.Crypto.Hmac as Hmac
import Sources.Crypto.Utils as Utils
import Sources.Processing.Types exposing (HttpMethod)
import Sources.Types exposing (SourceData)
import Sources.Services.Utils as Utils
import Utils


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
    -> Date
    -> SourceData
    -> String
    -> List ( String, String )
    -> String
presignedUrl storageMethod computation httpMethod hoursToLive currentDate srcData pathToFile params =
    let
        azure =
            srcData

        accountName =
            Dict.fetchUnknown "accountName" azure

        accountKey =
            Dict.fetchUnknown "accountKey" azure

        container =
            Dict.fetchUnknown "container" azure

        -- {var} Paths
        filePath =
            if String.isEmpty pathToFile then
                ""
            else if String.startsWith "/" pathToFile then
                pathToFile
            else
                "/" ++ pathToFile

        -- {var} Time
        expiryTime =
            currentDate
                |> Date.Extra.add Date.Extra.Hour hoursToLive
                |> Date.Extra.toUtcFormattedString "y-MM-ddTHH:mmZ"

        -- {var} Other
        permissions =
            case computation of
                List ->
                    "l"

                Read ->
                    "r"

        resourceType =
            storageMethod
                |> toString
                |> String.toLower

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
            , accountName
            , "."
            , resourceType
            , ".core.windows.net/"
            , container
            , filePath

            -- Start query params
            , "?"
            , params
                |> List.map Utils.makeQueryParam
                |> String.join "&"

            -- Query params for certain requests
            , case computation of
                List ->
                    "&restype=" ++ resType ++ "&comp=list"

                _ ->
                    ""

            -- Signature things
            , "&sv="
            , Utils.encodeUri signatureStuff.version
            , "&ss="
            , Utils.encodeUri signatureStuff.services
            , "&srt="
            , Utils.encodeUri signatureStuff.resources
            , "&sp="
            , Utils.encodeUri signatureStuff.permissions
            , "&se="
            , Utils.encodeUri signatureStuff.expiryTime
            , "&spr="
            , Utils.encodeUri signatureStuff.protocol
            , "&sig="
            , Utils.encodeUri (makeSignature signatureStuff)
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
            [ accountName
            , permissions
            , services
            , resources
            , startTime
            , expiryTime
            , ""
            , protocol
            , version
            ]
                |> String.join "\n"
                |> (\str -> str ++ "\n")
    in
        accountKey
            |> BinaryBase64.decode
            |> Result.withDefault []
            |> Utils.byteArrayToString
            |> Hmac.encrypt64 SHA.sha256 message
            |> Utils.stringToByteArray
            |> BinaryBase64.encode
