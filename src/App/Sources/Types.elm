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


type alias SourceId =
    String


type alias Source =
    { id : SourceId
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
    , receivedTags : List (Maybe Tags)
    , sourceId : String
    , urlsForTags : List TagUrls
    }


type alias CmdWithTimestamp =
    Date -> Cmd Msg


type alias TreeStepResult =
    Result Http.Error String



-- Other types


type alias Model =
    { collection : List Source
    , isProcessing : Maybe (List Source)
    , newSource : Source
    , processingError : Maybe String
    , timestamp : Date
    }


type Msg
    = Process
    | ProcessNextInLine
    | ProcessTreeStep ProcessingContext TreeStepResult
    | ProcessTagsStep ProcessingContextForTags
      -- CRUD
    | Destroy SourceId
      -- Forms
    | SetNewSourceProperty Source String String
    | SubmitNewSourceForm


type Page
    = Index
    | New



-- 🌱


makeSource : Service -> SourceData -> Source
makeSource service data =
    { id = "change_me_please"
    , data = data
    , service = service
    }
