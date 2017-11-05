module Sources.Types exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
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
    , directoryPlaylists : Bool
    , enabled : Bool
    , service : Service
    }



-- Messages


type Msg
    = -- CRUD
      Destroy SourceId
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
    , processingErrors : List ( SourceId, String ) -- TODO
    , timestamp : Date
    }



-- Pages


type Page
    = Edit SourceId
    | Index
    | New



-- Other Types


type Form
    = NewForm Int Source
    | EditForm Source


type alias IsProcessing =
    Maybe (List Source)
