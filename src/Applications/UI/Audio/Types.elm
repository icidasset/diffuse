module UI.Audio.Types exposing (..)

import Queue
import Tracks exposing (IdentifiedTrack)


type AudioLoadingState
    = Loading
    | Loaded
      -- Errors
    | Stalled
    | Aborted
    | DecodingError
    | NetworkError
    | NotSupportedOrMissing
    | UnknownError


type alias NowPlaying =
    { duration : Maybe Float
    , isPlaying : Bool
    , item : Queue.Item
    , loadingState : AudioLoadingState
    , playbackPosition : Float
    }



-- 🛠️


nowPlayingIdentifiedTrack : NowPlaying -> IdentifiedTrack
nowPlayingIdentifiedTrack { item } =
    item.identifiedTrack
