module Sources exposing (Page(..), Service(..), Source, SourceData)

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



-- PAGE


type Page
    = Index
