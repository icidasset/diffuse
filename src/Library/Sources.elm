module Sources exposing (Form, FormStep(..), Page(..), Service(..), Source, SourceData, defaultService, emptySource, newForm)

import Dict exposing (Dict)



-- 🌳


type alias Source =
    { id : String
    , data : SourceData
    , directoryPlaylists : Bool
    , enabled : Bool
    , service : Service
    }



-- PIECES


type alias SourceData =
    Dict String String


type Service
    = AmazonS3


defaultService : Service
defaultService =
    AmazonS3



-- PAGE


type Page
    = Index
    | New Form



-- FORM


type alias Form =
    { step : FormStep, context : Source }


type FormStep
    = Where
    | How
    | By


newForm : Form
newForm =
    { step = Where, context = emptySource }



-- ⚡️


emptySource : Source
emptySource =
    { id = "CHANGE_ME_PLEASE"
    , data = Dict.empty
    , directoryPlaylists = True
    , enabled = True
    , service = defaultService
    }
