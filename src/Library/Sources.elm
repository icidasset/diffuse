module Sources exposing (Property, Service(..), Source, SourceData, enabledSourceIds, setProperId)

import Conditional exposing (..)
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
    { key : String
    , label : String
    , placeholder : String
    , password : Bool
    }


type alias SourceData =
    Dict String String



-- SERVICES


type Service
    = AmazonS3
    | AzureBlob
    | AzureFile
    | Btfs
    | Dropbox
    | Google
    | Ipfs
    | WebDav



--- 🔱


enabledSourceIds : List Source -> List String
enabledSourceIds =
    List.filterMap (\s -> ifThenElse s.enabled (Just s.id) Nothing)


setProperId : Int -> Time.Posix -> Source -> Source
setProperId n time source =
    { source | id = String.fromInt (Time.posixToMillis time) ++ String.fromInt n }
