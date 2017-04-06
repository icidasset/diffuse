port module Sources.Ports exposing (..)

import Sources.Types exposing (..)
import Tracks.Types exposing (Track)


-- 💡


port requestTags : ProcessingContextForTags -> Cmd msg


port storeSources : List SourceReplica -> Cmd msg


port storeTracks : List Track -> Cmd msg



-- 🚽


port receiveTags : (ProcessingContextForTags -> msg) -> Sub msg
