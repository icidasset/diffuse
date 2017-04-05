module Sources.Types exposing (..)

import Date exposing (Date)
import Http
import Tracks.Types exposing (..)


-- Services

import Sources.Services.AmazonS3.Types as AmazonS3 exposing (..)


-- Sources


type SourceData
    = AmazonS3 AmazonS3Source


type alias Source =
    { id : String
    , data : SourceData
    }


newSource : SourceData -> Source
newSource data =
    { id = "change_me_please"
    , data = data
    }



-- Processing


type HttpMethod
    = Get
    | Head


type Marker
    = TheBeginning
    | InProgress String
    | TheEnd


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
    | ProcessInsertionStep Source ProcessingContextForTags
      -- Firebase
    | SyncSources
    | SyncTracks
      -- Forms
    | SetNewSourceProperty Source String String
    | SubmitNewSourceForm


type Page
    = Index
    | New
