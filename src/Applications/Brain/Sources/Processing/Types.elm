module Brain.Sources.Processing.Types exposing (..)

import Alien
import Dict.Ext as Dict
import Http exposing (Error(..))
import Json.Decode as Json
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Sources exposing (Service, Source)
import Sources.Processing exposing (..)
import Sources.Services as Services
import Time
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
