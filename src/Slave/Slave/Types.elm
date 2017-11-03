module Slave.Types exposing (..)

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
    | SetTimestamp



-- Talking to the outside world


type AlienMsg
    = -- Tracks
      AddTracks
    | RemoveTracksByPath
      -- Other
    | ProcessSources


type alias AlienResult =
    Result String Json.Encode.Value
