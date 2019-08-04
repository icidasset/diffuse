module Brain.Reply exposing (Reply(..))

import Alien
import Json.Encode as Json
import Tracks exposing (Track)
import User.Layer



-- 🌳


type Reply
    = FabricatedNewSecretKey
    | ImportHypaethralData User.Layer.HypaethralData
      -- Tracks
    | AddTracks (List Track)
    | RemoveTracksByPaths { sourceId : String, paths : List String }
      -- UI
    | GiveUI Alien.Tag Json.Value
    | NudgeUI Alien.Tag
