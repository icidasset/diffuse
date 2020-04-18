module Brain.Sources.Processing.Types exposing (..)

import Http exposing (Error(..))
import Json.Decode as Json
import Sources.Processing exposing (..)
import Tracks exposing (Track)



-- 📣


type Msg
    = Process Json.Value
    | NextInLine
    | StopProcessing
      -----------------------------------------
      -- Steps
      -----------------------------------------
    | PrepareStep Context (Result Http.Error String)
    | TreeStep Context (Result Http.Error String)
    | TreeStepRemoveTracks String (List String)
    | TagsStep ContextForTags
