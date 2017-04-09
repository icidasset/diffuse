module Sources.Services.AmazonS3.Parser exposing (parseTreeResponse)

import List.Extra
import Regex exposing (HowMany(All), regex)
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
                |> List.map (unescape)

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



-- Private


{-| String replacement.
-}
replace : String -> String -> String -> String
replace needle replacement haystack =
    Regex.replace All (regex needle) (always replacement) haystack


{-| Unescape these codes specific to XML.
-}
unescape : String -> String
unescape =
    replace "&apos;" "'"
        >> replace "&quot;" "\""
        >> replace "&lt;" "<"
        >> replace "&gt;" ">"
        >> replace "&amp;" "&"
