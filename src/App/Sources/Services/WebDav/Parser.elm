module Sources.Services.WebDav.Parser exposing (parseTreeResponse, parseErrorResponse)

import Maybe.Extra as Maybe
import Regex
import Sources.Pick exposing (isMusicFile)
import Sources.Processing.Types exposing (Marker(..), TreeAnswer)
import Sources.Services.Utils exposing (unescapeXmlEntities)
import Sources.Services.WebDav.Marker as Marker


-- Tree


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse response previousMarker =
    let
        responseTags =
            response
                |> Regex.replace Regex.All (Regex.regex docTag) (always "")
                |> Regex.replace Regex.All (Regex.regex startRootTag) (always "")
                |> Regex.replace Regex.All (Regex.regex endRootTag) (always "")
                |> String.split "</d:response>"
                |> List.drop 1
                |> List.map (\s -> s ++ "</d:response>")

        ( rawDirs, rawFiles ) =
            List.partition isDir responseTags

        dirs =
            List.map getHref rawDirs

        files =
            rawFiles
                |> List.filter isAudioFile
                |> List.map getHref
                |> List.filter isMusicFile
    in
        { filePaths =
            files

        -- files
        , marker =
            previousMarker
                |> Marker.removeOne
                |> Marker.concat dirs
        }


docTag : String
docTag =
    "\\s*" ++ Regex.escape "<?xml version=\"1.0\"?>" ++ "\\s*"


startRootTag : String
startRootTag =
    "<d:multistatus[^>]*>"


endRootTag : String
endRootTag =
    Regex.escape "</d:multistatus>"


getHref : String -> String
getHref xmlString =
    xmlString
        |> Regex.find (Regex.AtMost 1) (Regex.regex "<d:href>([^<]+)</d:href>")
        |> List.head
        |> Maybe.map .submatches
        |> Maybe.andThen List.head
        |> Maybe.join
        |> Maybe.map unescapeXmlEntities
        |> Maybe.withDefault "invalidHref"


isAudioFile : String -> Bool
isAudioFile xmlString =
    String.contains "<d:getcontenttype>audio/" xmlString


isDir : String -> Bool
isDir xmlString =
    Regex.contains (Regex.regex "<d:collection\\s*/>") xmlString



-- Error


parseErrorResponse : String -> String
parseErrorResponse =
    identity
