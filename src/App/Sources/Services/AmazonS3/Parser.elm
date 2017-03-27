module Sources.Services.AmazonS3.Parser exposing (parseResponse)

import List.Extra
import Sources.Services.AmazonS3.Types exposing (ParsedResponse)
import Sources.Types exposing (Marker(..))
import Xml
import Xml.Encode as Xml
import Xml.Decode as Xml
import Xml.Query exposing (..)


parseResponse : String -> ParsedResponse Marker
parseResponse response =
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
