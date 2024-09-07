module UI.Sources.Types exposing (..)

import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Json
import Sources exposing (..)



-- 🌳


type alias Form =
    { context : Source
    , step : FormStep
    }


type FormStep
    = Where
    | How
    | By



-- 📣


type Msg
    = Bypass
      --
    | FinishedProcessingSource Json.Value
    | FinishedProcessing
    | Process
    | ProcessSpecific (List Source)
    | ReportProcessingError Json.Value
    | ReportProcessingProgress Json.Value
    | StopProcessing
      -----------------------------------------
      -- Collection
      -----------------------------------------
    | AddToCollection Source
    | RemoveFromCollection { sourceId : String }
    | UpdateSourceData Json.Value
      -----------------------------------------
      -- Form
      -----------------------------------------
    | AddSourceUsingForm
    | EditSourceUsingForm
    | RenameSourceUsingForm
    | ReturnToIndex
    | SelectService String
    | SetFormData String String
    | TakeStep
    | TakeStepBackwards
      -----------------------------------------
      -- Individual
      -----------------------------------------
    | SourceContextMenu Source Mouse.Event
    | ToggleActivation { sourceId : String }
    | ToggleDirectoryPlaylists { sourceId : String }
    | ToggleProcessAutomatically
