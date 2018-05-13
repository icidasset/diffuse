module Sources.Services.Azure.FileParser exposing (parseTreeResponse, parseErrorResponse)

import Dict.Ext as Dict
import Sources.Processing.Types exposing (Marker(..), ParsedResponse)
import Sources.Services.Azure.FileMarker as FileMarker exposing (MarkerItem(..))
import Sources.Services.Utils exposing (cleanPath, unescapeXmlEntities)
import Xml
import Xml.Encode as Xml
import Xml.Decode as Xml
import Xml.Query exposing (..)


-- Tree


parseTreeResponse : String -> Marker -> ParsedResponse Marker
parseTreeResponse response previousMarker =
    let
        decodedXml =
            response
                |> Xml.decode
                |> Result.withDefault Xml.null

        usedDirectory =
            case decodedXml of
                Xml.Object (_ :: (Xml.Tag "EnumerationResults" attributes _) :: _) ->
                    attributes
                        |> Dict.fetch "DirectoryPath" Xml.null
                        |> (\v ->
                                case v of
                                    Xml.StrNode str ->
                                        str

                                    _ ->
                                        ""
                           )
                        |> cleanPath

                _ ->
                    ""

        filePaths =
            decodedXml
                |> tag "Entries" (identity >> Ok)
                |> Result.withDefault Xml.null
                |> tags "File"
                |> collect (tag "Name" string)
                |> List.map (unescapeXmlEntities >> (++) usedDirectory)

        directoryPaths =
            decodedXml
                |> tag "Entries" (identity >> Ok)
                |> Result.withDefault Xml.null
                |> tags "Directory"
                |> collect (tag "Name" string)
                |> List.map (unescapeXmlEntities >> (++) usedDirectory)

        isTruncated =
            decodedXml
                |> tag "NextMarker" string
                |> Result.map (String.isEmpty >> not)
                |> Result.withDefault False

        markerWithDirectories =
            previousMarker
                |> FileMarker.removeOne
                |> FileMarker.concat (List.map Directory directoryPaths)

        marker =
            if isTruncated then
                decodedXml
                    |> tag "NextMarker" string
                    |> Result.map
                        (\mar ->
                            FileMarker.concat
                                [ Param { directory = usedDirectory, marker = mar } ]
                                markerWithDirectories
                        )
                    |> Result.withDefault markerWithDirectories
            else
                markerWithDirectories
    in
        { filePaths = filePaths
        , marker = marker
        }



-- Error


parseErrorResponse : String -> String
parseErrorResponse response =
    let
        decodedXml =
            response
                |> Xml.decode
                |> Result.withDefault Xml.null
    in
        decodedXml
            |> tags "Error"
            |> collect (tag "Message" string)
            |> List.head
            |> Maybe.withDefault "Invalid request"
