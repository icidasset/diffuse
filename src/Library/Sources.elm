module Sources exposing (Page(..), Property, Service(..), Source, SourceData, setProperId)

import Dict exposing (Dict)
import Time



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



--- 🔱


setProperId : Int -> Time.Posix -> Source -> Source
setProperId n time source =
    { source | id = String.fromInt (Time.toMillis Time.utc time) ++ String.fromInt n }
