module UI.Audio.Types exposing (..)

import Queue
import Tracks exposing (IdentifiedTrack)



-- 🌳


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



-- 🌳  ░░  EVENTS


type alias DurationChangeEvent =
    { trackId : String, duration : Float }


type alias GenericAudioEvent =
    { trackId : String }


type alias PlaybackStateEvent =
    { trackId : String, isPlaying : Bool }


type alias TimeUpdatedEvent =
    { trackId : String, currentTime : Float, duration : Maybe Float }



-- 🛠️


nowPlayingIdentifiedTrack : NowPlaying -> IdentifiedTrack
nowPlayingIdentifiedTrack { item } =
    item.identifiedTrack
