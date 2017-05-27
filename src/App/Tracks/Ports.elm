port module Tracks.Ports exposing (..)

import Json.Encode as Json
import Tracks.Types exposing (..)


-- 💡


port performSearch : String -> Cmd msg


port storeTracksSettings : Settings -> Cmd msg


port updateSearchIndex : List Json.Value -> Cmd msg



-- 🚽


port receiveSearchResults : (List SourceId -> msg) -> Sub msg
