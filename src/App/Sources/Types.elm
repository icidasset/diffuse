module Sources.Types exposing (..)

import Date exposing (Date)
import Http
import Tracks.Types exposing (..)


-- Services

import Sources.Services.AmazonS3.Types as AmazonS3 exposing (..)


-- Sources & Processing


type Source
    = AmazonS3 AmazonS3Source


type Marker
    = TheBeginning
    | InProgress String
    | TheEnd


type HttpMethod
    = Get
    | Head


type alias ProcessingContext =
    { filePaths : List String
    , source : Source
    , treeMarker : Marker
    }


type alias ProcessingContextForTags =
    { filePaths : List String
    , receivedTags : List Tags
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
    , timestamp : Date
    }


type Msg
    = Process
    | ProcessNextInLine
    | ProcessTreeStep ProcessingContext TreeStepResult
    | ProcessTagsStep ProcessingContextForTags
    | ProcessInsertionStep Source ProcessingContextForTags
      -- Forms
    | SetNewSource Source
    | SetNewSourceProperty Source String String
    | SubmitNewSourceForm


type Page
    = Index
    | New
