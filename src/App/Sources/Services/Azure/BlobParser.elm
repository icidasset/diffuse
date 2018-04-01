module Sources.Services.Azure.BlobParser exposing (parseTreeResponse, parseErrorResponse)

import Sources.Processing.Types exposing (Marker(..), ParsedResponse)
import Sources.Services.Utils exposing (unescapeXmlEntities)
import Xml
import Xml.Encode as Xml
import Xml.Decode as Xml
import Xml.Query exposing (..)


-- Tree


parseTreeResponse : String -> Marker -> ParsedResponse Marker
parseTreeResponse response _ =
    let
        decodedXml =
            response
                |> Xml.decode
                |> Result.withDefault Xml.null

        filePaths =
            decodedXml
                |> tag "Blobs" (identity >> Ok)
                |> Result.withDefault Xml.null
                |> tags "Blob"
                |> collect (tag "Name" string)
                |> List.map unescapeXmlEntities

        isTruncated =
            decodedXml
                |> tag "NextMarker" string
                |> Result.map (String.isEmpty >> not)
                |> Result.withDefault False

        marker =
            if isTruncated then
                decodedXml
                    |> tag "NextMarker" string
                    |> Result.map InProgress
                    |> Result.withDefault TheEnd
            else
                TheEnd
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
