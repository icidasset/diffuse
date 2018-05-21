module Sources.Processing.Types exposing (..)

import Date exposing (Date)
import Http
import Sources.Types exposing (Source, SourceData)
import Tracks.Types exposing (..)


-- Messages


type Msg
    = Process String (List Source) (List Track)
    | NextInLine
    | PrepareStep Context (Result Http.Error String)
    | TreeStep Context (Result Http.Error String)
    | TreeStepRemoveTracks SourceId (List String)
    | TagsStep ContextForTags



-- Model


type alias Model =
    { origin : String
    , status : Status
    , timestamp : Date
    }



-- Markers & Responses


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



-- Contexts


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



-- Other


type HttpMethod
    = Get
    | Head


type alias Status =
    Maybe (List ( Source, List Track ))
