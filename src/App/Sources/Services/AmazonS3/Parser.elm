module Sources.Services.AmazonS3.Parser exposing (parseTreeResponse, parseErrorResponse)

import Sources.Processing.Types exposing (Marker(..), TreeAnswer)
import Sources.Services.Utils exposing (unescapeXmlEntities)
import Xml
import Xml.Encode as Xml
import Xml.Decode as Xml
import Xml.Query exposing (..)


-- Tree


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response _ =
    let
        decodedXml =
            response
                |> Xml.decode
                |> Result.withDefault Xml.null

        filePaths =
            decodedXml
                |> tags "Contents"
                |> collect (tag "Key" string)
                |> List.map unescapeXmlEntities

        isTruncated =
            decodedXml
                |> tag "IsTruncated" string
                |> Result.withDefault ""

        marker =
            if isTruncated == "true" then
                decodedXml
                    |> tag "NextContinuationToken" string
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
