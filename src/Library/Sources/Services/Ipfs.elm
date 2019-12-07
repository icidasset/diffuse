module Sources.Services.Ipfs exposing (..)

{-| IPFS Service.

Resources:

  - <https://ipfs.io/docs/api/>

-}

import Common exposing (boolFromString, boolToString)
import Conditional exposing (ifThenElse)
import Dict
import Dict.Ext as Dict
import Http
import Json.Decode as Json
import Sources exposing (Property, SourceData)
import Sources.Processing exposing (..)
import Sources.Services.Ipfs.Marker as Marker
import Sources.Services.Ipfs.Parser as Parser
import String.Ext as String
import Task
import Time
import Url



-- PROPERTIES
-- 📟


defaults =
    { gateway = ""
    , local = boolToString False
    , name = "Music from IPFS"
    , ipns = boolToString False
    }


defaultGateway =
    "http://127.0.0.1:8080"


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List Property
properties =
    [ { key = "directoryHash"
      , label = "Directory hash / DNSLink domain"
      , placeholder = "QmVLDAhCY3X9P2u"
      , password = False
      }
    , { key = "ipns"
      , label = "Resolve using IPNS"
      , placeholder = defaults.ipns
      , password = False
      }
    , { key = "gateway"
      , label = "Gateway (Optional)"
      , placeholder = defaultGateway
      , password = False
      }
    , { key = "local"
      , label = "Resolve IPNS locally"
      , placeholder = defaults.local
      , password = False
      }
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "directoryHash", "" )
        , ( "gateway", defaults.gateway )
        , ( "ipns", defaults.ipns )
        , ( "local", defaults.local )
        , ( "name", defaults.name )
        ]



-- PREPARATION


prepare : String -> SourceData -> Marker -> (Result Http.Error String -> msg) -> Maybe (Cmd msg)
prepare _ srcData _ toMsg =
    let
        domainName =
            srcData
                |> Dict.get "directoryHash"
                |> Maybe.withDefault ""
                |> String.chopStart "http://"
                |> String.chopStart "https://"
                |> String.chopEnd "/"
                |> String.chopStart "_dnslink."
    in
    if isDnsLink srcData then
        (Just << Http.request)
            { method = "GET"
            , headers = []
            , url = extractGateway srcData ++ "/api/v0/dns?arg=" ++ domainName
            , body = Http.emptyBody
            , expect = Http.expectString toMsg
            , timeout = Nothing
            , tracker = Nothing
            }

    else
        Nothing



-- TREE


{-| Create a directory tree.
-}
makeTree : SourceData -> Marker -> Time.Posix -> (Result Http.Error String -> msg) -> Cmd msg
makeTree srcData marker _ resultMsg =
    let
        gateway =
            extractGateway srcData

        resolveWithIpns =
            case marker of
                InProgress _ ->
                    False

                _ ->
                    srcData
                        |> Dict.fetch "ipns" defaults.ipns
                        |> boolFromString

        resolveLocally =
            srcData
                |> Dict.fetch "local" defaults.local
                |> boolFromString
                |> (\b -> ifThenElse b "true" "false")

        root =
            rootHash srcData

        path =
            case marker of
                InProgress _ ->
                    marker
                        |> Marker.takeOne
                        |> Maybe.map (\p -> root ++ "/" ++ p)
                        |> Maybe.withDefault ""

                _ ->
                    root
    in
    (if resolveWithIpns then
        Http.task
            { method = "GET"
            , headers = []
            , url = gateway ++ "/api/v0/name/resolve?arg=" ++ encodedPath path ++ "&local=" ++ resolveLocally ++ "&encoding=json"
            , body = Http.emptyBody
            , resolver = Http.stringResolver ipnsResolver
            , timeout = Just (60 * 15 * 1000)
            }

     else
        Task.succeed { ipfsPath = path }
    )
        |> Task.andThen
            (\{ ipfsPath } ->
                Http.task
                    { method = "GET"
                    , headers = []
                    , url = gateway ++ "/api/v0/ls?arg=" ++ encodedPath ipfsPath ++ "&encoding=json"
                    , body = Http.emptyBody
                    , resolver = Http.stringResolver Common.translateHttpResponse
                    , timeout = Just (60 * 15 * 1000)
                    }
            )
        |> Task.attempt resultMsg


ipnsResolver : Http.Response String -> Result Http.Error { ipfsPath : String }
ipnsResolver response =
    case response of
        Http.BadUrl_ u ->
            Err (Http.BadUrl u)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ _ body ->
            Err (Http.BadBody body)

        Http.GoodStatus_ _ body ->
            body
                |> Json.decodeString (Json.field "Path" Json.string)
                |> Result.map (\path -> { ipfsPath = String.chopStart "/ipfs/" path })
                |> Result.mapError (Json.errorToString >> Http.BadBody)


{-| Re-export parser functions.
-}
parsePreparationResponse : String -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse =
    Parser.parseDnsLookup


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse =
    Parser.parseTreeResponse


parseErrorResponse : String -> Maybe String
parseErrorResponse =
    Parser.parseErrorResponse



-- POST


{-| Post process the tree results.

!!! Make sure we only use music files that we can use.

-}
postProcessTree : List String -> List String
postProcessTree =
    identity



-- TRACK URL


{-| Create a public url for a file.

We need this to play the track.

-}
makeTrackUrl : Time.Posix -> SourceData -> HttpMethod -> String -> String
makeTrackUrl _ srcData _ path =
    if not (String.contains "/" path) && not (String.contains "." path) then
        -- If it still uses the old way of doing things
        -- (ie. each path was a cid)
        extractGateway srcData ++ "/ipfs/" ++ path

    else
        -- Or the new way
        extractGateway srcData ++ "/ipfs/" ++ rootHash srcData ++ "/" ++ encodedPath path



-- ⚗️


encodedPath : String -> String
encodedPath path =
    path
        |> String.split "/"
        |> List.map Url.percentEncode
        |> String.join "/"


extractGateway : SourceData -> String
extractGateway srcData =
    srcData
        |> Dict.get "gateway"
        |> Maybe.map String.trim
        |> Maybe.andThen
            (\s ->
                case s of
                    "" ->
                        Nothing

                    _ ->
                        Just s
            )
        |> Maybe.map (String.chopEnd "/")
        |> Maybe.withDefault defaultGateway


isDnsLink : SourceData -> Bool
isDnsLink srcData =
    srcData
        |> Dict.get "directoryHash"
        |> Maybe.map pathIsDnsLink
        |> Maybe.withDefault False


pathIsDnsLink : String -> Bool
pathIsDnsLink =
    String.contains "."


rootHash : SourceData -> String
rootHash srcData =
    srcData
        |> Dict.get "directoryHash"
        |> Maybe.andThen
            (\path ->
                if pathIsDnsLink path then
                    Dict.get "directoryHashFromDnsLink" srcData

                else
                    Just path
            )
        |> Maybe.withDefault ""
        |> String.chopEnd "/"
