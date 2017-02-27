module Sources.Services.AmazonS3.Parser exposing (parseResponse)

import Debug
import Sources.Services.AmazonS3.Types exposing (ParsedResponse)
import Xml
import Xml.Encode as Xml
import Xml.Decode as Xml
import Xml.Query exposing (..)


parseResponse : String -> ParsedResponse
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

        marker =
            decodedXml
                |> tag "Marker" string
                |> Result.withDefault ""
    in
        { filePaths = Debug.log "files" filePaths
        , marker = Debug.log "marker" marker
        }
