module Sources.Services.Ipfs exposing (defaults, initialData, makeTrackUrl, makeTree, parseErrorResponse, parsePreparationResponse, parseTreeResponse, postProcessTree, prepare, properties)

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
import Sources.Services.Common exposing (cleanPath, noPrep)
import Sources.Services.Ipfs.Marker as Marker
import Sources.Services.Ipfs.Parser as Parser
import String.Ext as String
import Task
import Time



-- PROPERTIES
-- 📟


defaults =
    { gateway = "http://127.0.0.1:8080"
    , local = boolToString False
    , name = "Music from IPFS"
    , ipns = boolToString False
    }


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List Property
properties =
    [ { key = "directoryHash"
      , label = "Directory object hash / DNSLink domain"
      , placeholder = "QmVLDAhCY3X9P2u"
      , password = False
      }
    , { key = "ipns"
      , label = "Resolve using IPNS"
      , placeholder = defaults.ipns
      , password = False
      }
    , { key = "gateway"
      , label = "Gateway"
      , placeholder = defaults.gateway
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
        , ( "name", defaults.name )
        , ( "gateway", defaults.gateway )
        , ( "ipns", defaults.ipns )
        , ( "local", defaults.local )
        ]



-- PREPARATION


prepare : String -> SourceData -> Marker -> (Result Http.Error String -> msg) -> Maybe (Cmd msg)
prepare _ srcData _ toMsg =
    let
        isDnsLink =
            srcData
                |> Dict.get "directoryHash"
                |> Maybe.map (String.contains ".")

        domainName =
            srcData
                |> Dict.get "directoryHash"
                |> Maybe.withDefault ""
                |> String.chopStart "http://"
                |> String.chopStart "https://"
                |> String.chopEnd "/"
                |> String.chopStart "_dnslink."
                |> String.append "_dnslink."
    in
    case isDnsLink of
        Just True ->
            (Just << Http.request)
                { method = "GET"
                , headers = [ Http.header "Accept" "application/dns-json" ]
                , url = "https://cloudflare-dns.com/dns-query?type=TXT&name=" ++ domainName
                , body = Http.emptyBody
                , expect = Http.expectString toMsg
                , timeout = Nothing
                , tracker = Nothing
                }

        _ ->
            Nothing



-- TREE


{-| Create a directory tree.
-}
makeTree : SourceData -> Marker -> Time.Posix -> (Result Http.Error String -> msg) -> Cmd msg
makeTree srcData marker _ resultMsg =
    let
        gateway =
            srcData
                |> Dict.get "gateway"
                |> Maybe.withDefault defaults.gateway
                |> String.foldr
                    (\char acc ->
                        if String.isEmpty acc && char == '/' then
                            acc

                        else
                            String.cons char acc
                    )
                    ""

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

        hash =
            case marker of
                InProgress _ ->
                    marker
                        |> Marker.takeOne
                        |> Maybe.withDefault "MISSING_HASH"

                _ ->
                    srcData
                        |> Dict.get "directoryHash"
                        |> Maybe.andThen
                            (\h ->
                                if String.contains "." h then
                                    Dict.get "directoryHashFromDnsLink" srcData

                                else
                                    Just h
                            )
                        |> Maybe.withDefault "MISSING_HASH"
    in
    (if resolveWithIpns then
        Http.task
            { method = "GET"
            , headers = []
            , url = gateway ++ "/api/v0/name/resolve?arg=" ++ hash ++ "&local=" ++ resolveLocally ++ "&encoding=json"
            , body = Http.emptyBody
            , resolver = Http.stringResolver ipnsResolver
            , timeout = Just (60 * 15)
            }

     else
        Task.succeed { ipfsHash = hash }
    )
        |> Task.andThen
            (\{ ipfsHash } ->
                Http.task
                    { method = "GET"
                    , headers = []
                    , url = gateway ++ "/api/v0/ls?arg=" ++ ipfsHash ++ "&encoding=json"
                    , body = Http.emptyBody
                    , resolver = Http.stringResolver Common.translateHttpResponse
                    , timeout = Just (60 * 15)
                    }
            )
        |> Task.attempt resultMsg


ipnsResolver : Http.Response String -> Result Http.Error { ipfsHash : String }
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
                |> Result.map (\hash -> { ipfsHash = hash })
                |> Result.mapError (Json.errorToString >> Http.BadBody)


{-| Re-export parser functions.
-}
parsePreparationResponse : String -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse =
    Parser.parseCloudflareDnsResult


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
makeTrackUrl _ srcData _ hash =
    let
        gateway =
            srcData
                |> Dict.get "gateway"
                |> Maybe.withDefault defaults.gateway
                |> String.foldr
                    (\char acc ->
                        if String.isEmpty acc && char == '/' then
                            acc

                        else
                            String.cons char acc
                    )
                    ""
    in
    gateway ++ "/ipfs/" ++ hash
