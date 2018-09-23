module Brain.Reply exposing (Reply(..))

import Json.Encode as Json



-- 🌳


type Reply
    = Chill
      -- UI
    | HideLoadingScreen
    | LoadUserData Json.Value
