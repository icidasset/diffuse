module Sources.Services.AmazonS3.Parser exposing (parseTreeResponse, parseErrorResponse)

import Maybe.Extra as Maybe
import Regex exposing (HowMany(All), regex)
import Sources.Crypto.Hex
import Sources.Processing.Types exposing (Marker(..), ParsedResponse)
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
                |> tags "Contents"
                |> collect (tag "Key" string)
                |> List.map (unescape)

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



-- Private


{-| String replacement.
-}
replace : String -> String -> String -> String
replace needle replacement haystack =
    Regex.replace All (regex needle) (always replacement) haystack


{-| Unescape the "HTML entities".
-}
unescape : String -> String
unescape =
    replace "&apos;" "'"
        >> replace "&quot;" "\""
        >> replace "&lt;" "<"
        >> replace "&gt;" ">"
        >> replace "&amp;" "&"
        >> Regex.replace All (regex "&#x(\\w+);") hexEntityToUnicode


hexEntityToUnicode : Regex.Match -> String
hexEntityToUnicode match =
    match.submatches
        |> List.head
        |> Maybe.join
        |> Maybe.map Sources.Crypto.Hex.hexToUnicodeChar
        |> Maybe.map String.fromChar
        |> Maybe.withDefault ""
