module Sources.Processing exposing (Arguments, Context, ContextForTags, HttpMethod(..), Marker(..), PrepationAnswer, Status(..), TagUrls, TreeAnswer, httpMethod)

import Http
import Sources exposing (Source, SourceData)
import Tracks exposing (Tags, Track)



-- 🌳


type Status
    = Processing (List ( Source, List Track ))
    | NotProcessing


type alias Arguments =
    { origin : String
    , sources : List Source
    , tracks : List Track
    }



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



-- CONTEXTS


type alias Context =
    { filePaths : List String
    , origin : String
    , preparationMarker : Marker
    , source : Source
    , treeMarker : Marker
    }


type alias ContextForTags =
    { nextFilePaths : List String
    , receivedFilePaths : List String
    , receivedTags : List (Maybe Tags)
    , sourceId : String
    , urlsForTags : List TagUrls
    }


type alias TagUrls =
    { getUrl : String
    , headUrl : String
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
