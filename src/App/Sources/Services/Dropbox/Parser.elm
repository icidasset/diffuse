module Sources.Services.Dropbox.Parser exposing (parseTreeResponse, parseErrorResponse)

import Json.Decode exposing (..)
import Sources.Processing.Types exposing (Marker(..), ParsedResponse)


-- Tree


parseTreeResponse : String -> Marker -> ParsedResponse Marker
parseTreeResponse response _ =
    let
        paths =
            decodeString
                (field "entries" <| list <| field "path_display" string)
                response
    in
        { filePaths = Result.withDefault [] paths
        , marker = TheEnd
        }



-- Error


parseErrorResponse : String -> String
parseErrorResponse response =
    response
