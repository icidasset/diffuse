module Sources.Services.Azure.Authorization exposing (Computation(..), StorageMethod(..), presignedUrl)

import Base64
import Date exposing (Date)
import Date.Extra
import Dict
import Dict.Ext as Dict
import SHA
import Sources.Crypto.Hex as Hex
import Sources.Crypto.Hmac as Hmac
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
    -> String
presignedUrl storageMethod computation httpMethod hoursToLive currentDate srcData pathToFile =
    let
        azure =
            Dict.map (\_ v -> String.trim v) srcData

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

        --
        comp =
            case computation of
                List ->
                    "list"

                Read ->
                    ""

        permissions =
            case computation of
                List ->
                    "rl"

                Read ->
                    "r"

        resource =
            case storageMethod of
                Blob ->
                    case computation of
                        List ->
                            "c"

                        Read ->
                            "b"

                File ->
                    -- TODO
                    -- "s" for share
                    -- "f" for file
                    ""

        resourceType =
            storageMethod
                |> toString
                |> String.toLower

        version =
            "2017-07-29"

        -- Signature
        canonicalizedResource =
            String.join ""
                [ "/"
                , resourceType
                , "/"
                , accountName
                , "/"
                , container
                , filePath
                ]

        signatureMessage =
            -- signedpermissions + "\n" +
            -- signedstart + "\n" +
            -- signedexpiry + "\n" +
            -- canonicalizedresource + "\n" +
            -- signedidentifier + "\n" +
            -- signedip + "\n" +
            -- signedprotocol + "\n" +
            -- signedversion + "\n" +
            -- rscc + "\n" +
            -- rscd + "\n" +
            -- rsce + "\n" +
            -- rscl + "\n" +
            -- rsct
            [ permissions
            , ""
            , expiryTime
            , canonicalizedResource
            , ""
            , ""
            , ""
            , version
            , ""
            , ""
            , ""
            , ""
            , ""
            ]
                |> Debug.log "signatureMessage"
                |> String.join "\n"

        signature =
            accountKey
                |> Base64.decode
                |> Result.withDefault ""
                |> Hmac.encrypt64 SHA.sha256sum signatureMessage
                |> Base64.encode
    in
        String.concat
            [ "https://"
            , accountName
            , "."
            , resourceType
            , ".core.windows.net/"
            , container
            , filePath
            , "?comp="
            , Utils.encodeUri comp
            , "&sv="
            , Utils.encodeUri version
            , "&se="
            , Utils.encodeUri expiryTime
            , "&sp="
            , Utils.encodeUri permissions
            , "&sr="
            , Utils.encodeUri resource
            , "&sig="
            , Utils.encodeUri signature
            ]
