module Sources.Types exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Http
import Json.Encode
import Tracks.Types exposing (..)


-- Sources


type Service
    = AmazonS3
    | Ipfs


type alias SourceData =
    Dict String String


type alias SourceId =
    String


type alias Source =
    { id : SourceId
    , data : SourceData
    , enabled : Bool
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


type alias IsProcessing =
    Maybe (List ( Source, List Track ))



-- Messages


type Msg
    = Process (List Track)
    | ProcessNextInLine
    | ProcessTreeStep ProcessingContext TreeStepResult
    | ProcessTreeStepRemoveTracks SourceId (List String)
    | ProcessTagsStep ProcessingContextForTags
      -- CRUD
    | Destroy SourceId
      -- Forms
    | AssignFormProperty String String
    | AssignFormService String
    | AssignFormStep Int
    | SubmitForm
      -- Other
    | ToggleSource Source



-- Model


type alias Model =
    { collection : List Source
    , form : Form
    , isProcessing : IsProcessing
    , processingErrors : List ( SourceId, String )
    , timestamp : Date
    }



-- Other Types


type Form
    = NewForm Int Source
    | EditForm Source


type Page
    = Edit SourceId
    | Index
    | New
