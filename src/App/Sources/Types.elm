module Sources.Types exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Json.Encode
import Tracks.Types exposing (..)


-- Sources


type Service
    = AmazonS3


type alias SourceData =
    Dict String String


type alias Source =
    { id : String
    , data : SourceData
    , service : Service
    }



-- Processing


type HttpMethod
    = Get
    | Head


type Marker
    = TheBeginning
    | InProgress String
    | TheEnd


type alias ParsedResponse marker =
    { filePaths : List String
    , marker : marker
    }


type alias ProcessingContext =
    { filePaths : List String
    , source : Source
    , treeMarker : Marker
    }


type alias ProcessingContextForTags =
    { nextFilePaths : List String
    , receivedFilePaths : List String
    , receivedTags : List Tags
    , sourceId : String
    , urlsForTags : List TagUrls
    }


type alias CmdWithTimestamp =
    Date -> Cmd Msg


type alias TreeStepResult =
    Result Http.Error String



-- Other types


type alias Model =
    { isProcessing : Maybe (List Source)
    , newSource : Source
    , processingError : Maybe String
    , sources : List Source
    , tracks : List Track
    , timestamp : Date
    }


type Msg
    = Process
    | ProcessNextInLine
    | ProcessTreeStep ProcessingContext TreeStepResult
    | ProcessTagsStep ProcessingContextForTags
    | ProcessInsertionStep ProcessingContextForTags
      -- Firebase
    | SyncSources
    | SyncTracks
      -- Forms
    | SetNewSourceProperty Source String String
    | SubmitNewSourceForm


type Page
    = Index
    | New
