module Sources.Services.WebDav.Parser exposing (..)

import Maybe.Extra as Maybe
import Sources.Pick exposing (isMusicFile)
import Sources.Processing exposing (Marker, TreeAnswer)
import Sources.Services.Ipfs.Marker as Marker
import String.Ext as String
import Url
import Xml.Decode exposing (..)
import XmlParser



-- TREE


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response previousMarker =
    let
        currentDir =
            Maybe.withDefault "//" (Marker.takeOne previousMarker)

        parseResult =
            XmlParser.parse response

        namespace =
            parseResult
                |> Result.map
                    (\xml ->
                        case xml.root of
                            XmlParser.Element nodeName _ _ ->
                                nodeName
                                    |> String.split ":"
                                    |> List.head

                            _ ->
                                Nothing
                    )
                |> Result.withDefault Nothing
                |> (\maybe ->
                        case maybe of
                            Just n ->
                                n ++ ":"

                            Nothing ->
                                if String.contains "<d:" response then
                                    "d:"

                                else
                                    "D:"
                   )

        entries =
            response
                |> decodeString (treeDecoder namespace)
                |> Result.withDefault []
                |> List.map Url.percentDecode
                |> Maybe.values
                |> List.filter ((/=) currentDir)

        ( dirs, files ) =
            List.partition (String.endsWith "/") entries
    in
    { filePaths =
        List.map (String.chopStart "/") files
    , marker =
        previousMarker
            |> Marker.removeOne
            |> Marker.concat dirs
    }


treeDecoder : String -> Decoder (List String)
treeDecoder namespace =
    path
        [ namespace ++ "response" ]
        (leakyList <| treeItemDecoder namespace)


treeItemDecoder : String -> Decoder String
treeItemDecoder namespace =
    let
        withNamespace =
            String.append namespace
    in
    string
        |> single
        |> path [ withNamespace "href" ]
        |> andThen
            (\href ->
                oneOf
                    [ -- Audio
                      --------
                      string
                        |> single
                        |> path [ withNamespace "propstat", withNamespace "prop", withNamespace "getcontenttype" ]
                        |> andThen (mustBeAudio href)
                        |> map (\_ -> href)

                    -- Directory
                    ------------
                    , string
                        |> single
                        |> path [ withNamespace "propstat", withNamespace "prop", withNamespace "resourcetype", withNamespace "collection" ]
                        |> andThen
                            (\_ ->
                                if String.endsWith "/@eaDir/" href then
                                    fail "Ignore Synology metadata"

                                else
                                    succeed href
                            )
                    ]
            )


mustBeAudio : String -> String -> Decoder String
mustBeAudio href contentType =
    if isMusicFile href then
        succeed contentType

    else if String.startsWith "audio/" contentType then
        succeed contentType

    else
        fail "Ignore this, not an audio file"



-- ERROR


parseErrorResponse : String -> Maybe String
parseErrorResponse =
    Just
