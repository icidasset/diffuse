module Brain.Reply exposing (Reply(..))

import Json.Encode as Json



-- 🌳


type Reply
    = Chill
      -- UI
    | HideLoadingScreen
    | LoadEnclosedUserData Json.Value
    | LoadHypaethralUserData Json.Value
    | ReportSourceProcessingError Json.Value
