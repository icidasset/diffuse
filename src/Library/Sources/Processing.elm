module Sources.Processing exposing (HttpMethod(..), Marker(..), PrepationAnswer, TreeAnswer, httpMethod)

import Http
import Sources exposing (SourceData)



-- MARKERS & RESPONSES


type Marker
    = TheBeginning
    | InProgress String
    | TheEnd


type alias PrepationAnswer marker =
    { sourceData : SourceData
    , marker : marker
    }


type alias TreeAnswer marker =
    { filePaths : List String
    , marker : marker
    }



-- HTTP


type HttpMethod
    = Get
    | Head


httpMethod : HttpMethod -> String
httpMethod method =
    case method of
        Get ->
            "GET"

        Head ->
            "HEAD"
