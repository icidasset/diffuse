module Sources.Services.WebDav.Parser exposing (parseErrorResponse, parseTreeResponse)

import Maybe.Extra as Maybe
import Regex exposing (HowMany(..), Regex, regex)
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
                |> Regex.replace All docTag (always "")
                |> Regex.replace All startRootTag (always "")
                |> Regex.replace All endRootTag (always "")
                |> Regex.split All endResponseTag
                |> List.drop 1
                |> List.filter (Regex.contains startResponseTag)

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


getHref : String -> String
getHref xmlString =
    xmlString
        |> Regex.find (AtMost 1) hrefRegex
        |> List.head
        |> Maybe.map .submatches
        |> Maybe.andThen List.head
        |> Maybe.join
        |> Maybe.map unescapeXmlEntities
        |> Maybe.withDefault "INVALID_HREF"


isAudioFile : String -> Bool
isAudioFile xmlString =
    Regex.contains audioFileRegex xmlString


isDir : String -> Bool
isDir xmlString =
    Regex.contains dirRegex xmlString



-- Error


parseErrorResponse : String -> String
parseErrorResponse =
    identity



-- ⚗️
--
-- Regex (Tags)


docTag : Regex
docTag =
    regex "\\s*<\\?xml[^\\?>]*\\?>\\s*"


startRootTag : Regex
startRootTag =
    regex "<[dD]:multistatus[^>]*>"


endRootTag : Regex
endRootTag =
    regex "</[dD]:multistatus>"


startResponseTag : Regex
startResponseTag =
    regex "<[dD]:response>"


endResponseTag : Regex
endResponseTag =
    regex "</[dD]:response>"



-- ⚗️
--
-- Regex (Other)


audioFileRegex : Regex
audioFileRegex =
    regex "<[dD]:getcontenttype>audio/"


dirRegex : Regex
dirRegex =
    regex "<[dD]:collection[^/>]*/>"


hrefRegex : Regex
hrefRegex =
    regex "<[dD]:href>([^<]+)</[dD]:href>"
