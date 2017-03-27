module Sources.Types exposing (..)

import Date exposing (Date)
import Http


-- Services

import Sources.Services.AmazonS3.Types as AmazonS3 exposing (..)


-- Sources


type Source
    = AmazonS3 AmazonS3Source


type Marker
    = TheBeginning
    | InProgress String
    | TheEnd


type alias TreeStepResult =
    Result Http.Error String


type alias ProcessingContext =
    { filePaths : List String
    , source : Source
    , treeMarker : Marker
    }



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
    | ProcessTreeStep ProcessingContext TreeStepResult
    | ProcessTagsStep ProcessingContext
      -- Forms
    | SetNewSource Source
    | SetNewSourceProperty Source String String
    | SubmitNewSourceForm


type Page
    = Index
    | New
