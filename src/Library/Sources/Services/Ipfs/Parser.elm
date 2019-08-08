module Sources.Services.Ipfs.Parser exposing (Link, linkDecoder, parseCloudflareDnsResult, parseErrorResponse, parseTreeResponse, treeDecoder)

import Dict
import Json.Decode exposing (..)
import Sources exposing (SourceData)
import Sources.Pick exposing (isMusicFile)
import Sources.Processing exposing (Marker(..), PrepationAnswer, TreeAnswer)
import Sources.Services.Ipfs.Marker as Marker
import String.Ext as String



-- PREPARATION


parseCloudflareDnsResult : String -> SourceData -> Marker -> PrepationAnswer Marker
parseCloudflareDnsResult response srcData _ =
    case decodeString dnsResultDecoder response of
        Ok txt ->
            let
                dirHash =
                    txt
                        |> String.chopEnd "\""
                        |> String.chopStart "\""
                        |> String.chopStart "dnslink=/ipfs/"
            in
            srcData
                |> Dict.insert "directoryHashFromDnsLink" dirHash
                |> (\s -> { sourceData = s, marker = TheEnd })

        Err _ ->
            { sourceData = srcData, marker = TheEnd }


dnsResultDecoder : Decoder String
dnsResultDecoder =
    at [ "Answer", "0", "data" ] string



-- TREE


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response previousMarker =
    let
        links =
            case decodeString treeDecoder response of
                Ok l ->
                    l

                Err _ ->
                    []

        dirs =
            links
                |> List.filter (.typ >> (==) 1)
                |> List.map .hash

        files =
            links
                |> List.filter (.typ >> (==) 2)
                |> List.filter (.name >> isMusicFile)
                |> List.map .hash
    in
    { filePaths =
        files
    , marker =
        previousMarker
            |> Marker.removeOne
            |> Marker.concat dirs
    }


treeDecoder : Decoder (List Link)
treeDecoder =
    field "Objects" <| index 0 <| field "Links" <| list linkDecoder



-- LINKS


type alias Link =
    { hash : String
    , name : String
    , typ : Int
    }


linkDecoder : Decoder Link
linkDecoder =
    map3 Link
        (field "Hash" string)
        (field "Name" string)
        (field "Type" int)



-- ERRORS


parseErrorResponse : String -> Maybe String
parseErrorResponse response =
    response
        |> decodeString (field "Message" string)
        |> Result.toMaybe
