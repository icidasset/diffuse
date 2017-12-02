module Sources.Processing.Types exposing (..)

import Date exposing (Date)
import Http
import Sources.Types exposing (Source)
import Tracks.Types exposing (..)


-- Messages


type Msg
    = Process (List Source) (List Track)
    | NextInLine
    | TreeStep Context TreeStepResult
    | TreeStepRemoveTracks SourceId (List String)
    | TagsStep ContextForTags



-- Model


type alias Model =
    { status : Status
    , timestamp : Date
    }



-- Other


type HttpMethod
    = Get
    | Head


type Marker
    = TheBeginning
    | InProgress String
    | TheEnd


type alias Context =
    { filePaths : List String
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


type alias ParsedResponse marker =
    { filePaths : List String
    , marker : marker
    }


type alias Status =
    Maybe (List ( Source, List Track ))


type alias TreeStepResult =
    Result Http.Error String
