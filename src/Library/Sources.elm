module Sources exposing (Page(..), Property, Service(..), Source, SourceData)

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


type alias Property =
    { k : String -- Key
    , l : String -- Label
    , h : String -- Placeholder
    , p : Bool -- Password?
    }


type alias SourceData =
    Dict String String



-- SERVICES


type Service
    = AmazonS3



-- PAGE


type Page
    = Index
    | New
