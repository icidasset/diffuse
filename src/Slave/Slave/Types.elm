module Slave.Types exposing (..)

import Date exposing (Date)
import Json.Encode
import Time exposing (Time)


-- Children

import Sources.Processing.Types


-- Messages


type Msg
    = Extraterrestrial AlienMsg AlienResult
      --
      -- Children
    | SourceProcessingMsg Sources.Processing.Types.Msg
      --
      -- Time
    | SetTimestamp Time



-- Model


type alias Model =
    { sourceProcessing : Sources.Processing.Types.Model
    , timestamp : Date
    }



-- Talking to the outside world


type AlienMsg
    = -- Sources
      ProcessSources
    | ProcessSourcesCompleted
    | ReportProcessingError
    | UpdateSourceData
      -- Tracks
    | AddTracks
    | RemoveTracksByPath
      -- Other
    | ReportError


type alias AlienResult =
    Result String Json.Encode.Value



-- Other


type alias Illumination model childMsg =
    model -> List (Cmd childMsg) -> List (Cmd Msg) -> ( model, Cmd Msg )
