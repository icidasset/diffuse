module Sources.Services.AmazonS3.Parser exposing (..)

import List.Extra
import Sources.Types exposing (Marker(..), ParsedResponse)
import Xml
import Xml.Encode as Xml
import Xml.Decode as Xml
import Xml.Query exposing (..)


parseTreeResponse : String -> ParsedResponse Marker
parseTreeResponse response =
    let
        decodedXml =
            response
                |> Xml.decode
                |> Result.toMaybe
                |> Maybe.withDefault Xml.null

        filePaths =
            decodedXml
                |> tags "Contents"
                |> collect (tag "Key" string)

        isTruncated =
            decodedXml
                |> tag "IsTruncated" string
                |> Result.withDefault ""

        marker =
            if isTruncated == "true" then
                List.Extra.last filePaths
                    |> Maybe.map InProgress
                    |> Maybe.withDefault TheEnd
            else
                TheEnd
    in
        { filePaths = filePaths
        , marker = marker
        }
