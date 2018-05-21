module Sources.Services.Utils exposing (..)

import Maybe.Extra as Maybe
import Regex exposing (HowMany(All), regex)
import Sources.Crypto.Hex
import Sources.Types exposing (SourceData)
import Sources.Processing.Types exposing (..)


{-| Clean a path.

Cases:

  - "" -> ""
  - "/example" -> "example/"
  - "example" -> "example/"
  - "example/" -> "example/"

-}
cleanPath : String -> String
cleanPath dirtyPath =
    dirtyPath
        |> String.trim
        |> Regex.replace Regex.All (Regex.regex "(^\\/|\\/$)") (\_ -> "")
        |> (\d ->
                if String.isEmpty d then
                    d
                else
                    d ++ "/"
           )


{-| String replacement.
-}
replace : String -> String -> String -> String
replace needle replacement haystack =
    Regex.replace All (regex needle) (always replacement) haystack



-- Parsing


noPrep : String -> SourceData -> Marker -> PrepationAnswer Marker
noPrep _ srcData _ =
    { sourceData = srcData, marker = TheEnd }



-- XML


hexEntityToUnicode : Regex.Match -> String
hexEntityToUnicode match =
    match.submatches
        |> List.head
        |> Maybe.join
        |> Maybe.map Sources.Crypto.Hex.hexToUnicodeChar
        |> Maybe.map String.fromChar
        |> Maybe.withDefault ""


unescapeXmlEntities : String -> String
unescapeXmlEntities =
    replace "&apos;" "'"
        >> replace "&quot;" "\""
        >> replace "&lt;" "<"
        >> replace "&gt;" ">"
        >> replace "&amp;" "&"
        >> Regex.replace All (regex "&#x(\\w+);") hexEntityToUnicode
