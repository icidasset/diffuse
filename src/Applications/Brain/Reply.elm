module Brain.Reply exposing (Reply(..))

import Alien
import Json.Encode as Json



-- 🌳


type Reply
    = FabricatedNewSecretKey
      -- UI
    | GiveUI Alien.Tag Json.Value
    | NudgeUI Alien.Tag
