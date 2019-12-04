module Sources.Services.Ipfs.Parser exposing (Link, linkDecoder, parseDnsLookup, parseErrorResponse, parseTreeResponse, treeDecoder)

import Dict
import Json.Decode exposing (..)
import Sources exposing (SourceData)
import Sources.Pick exposing (isMusicFile)
import Sources.Processing exposing (Marker(..), PrepationAnswer, TreeAnswer)
import Sources.Services.Ipfs.Marker as Marker
import String.Ext as String



-- PREPARATION


parseDnsLookup : String -> SourceData -> Marker -> PrepationAnswer Marker
parseDnsLookup response srcData _ =
    case decodeString dnsResultDecoder response of
        Ok path ->
            srcData
                |> Dict.insert "directoryHashFromDnsLink" (String.chopStart "/ipfs/" path)
                |> (\s -> { sourceData = s, marker = TheEnd })

        Err _ ->
            { sourceData = srcData, marker = TheEnd }


dnsResultDecoder : Decoder String
dnsResultDecoder =
    oneOf
        [ at [ "Path" ] string
        , cloudflareDnsResultDecoder
        ]


cloudflareDnsResultDecoder : Decoder String
cloudflareDnsResultDecoder =
    string
        |> at [ "Answer", "0", "data" ]
        |> map
            (\txt ->
                txt
                    |> String.chopEnd "\""
                    |> String.chopStart "\""
                    |> String.chopStart "dnslink=/ipfs/"
            )



-- TREE


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response previousMarker =
    let
        prefix =
            case previousMarker of
                TheBeginning ->
                    ""

                _ ->
                    response
                        |> decodeString prefixDecoder
                        |> Result.map
                            (String.chopStart "/ipfs/"
                                >> String.split "/"
                                >> List.drop 1
                                >> String.join "/"
                            )
                        |> Result.map
                            (\s ->
                                if String.isEmpty s then
                                    ""

                                else
                                    s ++ "/"
                            )
                        |> Result.withDefault ""

        links =
            case decodeString treeDecoder response of
                Ok l ->
                    l

                Err _ ->
                    []

        dirs =
            links
                |> List.filter (.typ >> (==) 1)
                |> List.map (\l -> prefix ++ l.name)

        files =
            links
                |> List.filter (.typ >> (==) 2)
                |> List.filter (.name >> isMusicFile)
                |> List.map (\l -> prefix ++ l.name)
    in
    { filePaths =
        files
    , marker =
        previousMarker
            |> Marker.removeOne
            |> Marker.concat dirs
    }


prefixDecoder : Decoder String
prefixDecoder =
    field "Objects" <| index 0 <| field "Hash" <| string


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
