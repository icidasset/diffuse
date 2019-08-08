module Sources.Services.Dropbox.Parser exposing (parseErrorResponse, parseTreeResponse)

import Json.Decode exposing (..)
import Sources.Processing exposing (Marker(..), TreeAnswer)



-- TREE


parseTreeResponse : String -> Marker -> TreeAnswer Marker
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


parseErrorResponse : String -> Maybe String
parseErrorResponse response =
    response
        |> decodeString (field "error_summary" string)
        |> Result.toMaybe
