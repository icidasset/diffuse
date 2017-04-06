port module Sources.Ports exposing (..)

import Json.Encode as Json
import Sources.Types exposing (..)
import Tracks.Types exposing (Track)


-- 💡


port requestTags : ProcessingContextForTags -> Cmd msg


port storeSources : List Json.Value -> Cmd msg


port storeTracks : List Json.Value -> Cmd msg



-- 🚽


port receiveTags : (ProcessingContextForTags -> msg) -> Sub msg
