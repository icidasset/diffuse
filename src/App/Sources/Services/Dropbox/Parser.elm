module Sources.Services.Dropbox.Parser exposing (parseTreeResponse, parseErrorResponse)

import Json.Decode exposing (..)
import Sources.Processing.Types exposing (Marker(..), ParsedResponse)


-- Tree


parseTreeResponse : String -> Marker -> ParsedResponse Marker
parseTreeResponse response _ =
    let
        hasMore =
            decodeString (field "has_more" bool) response

        cursor =
            decodeString (field "cursor" string) response

        paths =
            decodeString
                (field "entries" <| list <| field "path_display" string)
                response
    in
        { filePaths = Result.withDefault [] paths
        , marker =
            if Result.withDefault False hasMore then
                InProgress (Result.withDefault "" cursor)
            else
                TheEnd
        }



-- Error


parseErrorResponse : String -> String
parseErrorResponse response =
    response
