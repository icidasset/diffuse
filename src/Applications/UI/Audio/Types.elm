module UI.Audio.Types exposing (..)

import Queue
import Tracks exposing (IdentifiedTrack)



-- 🌳


type AudioLoadingState
    = Loading
    | Loaded
      -- Errors
    | DecodeError
    | NetworkError
    | NotSupportedError


type alias CoverPrep =
    { cacheKey : String
    , trackFilename : String
    , trackPath : String
    , trackSourceId : String
    , variousArtists : String
    }


type alias NowPlaying =
    { coverLoaded : Bool
    , duration : Maybe Float
    , isPlaying : Bool
    , item : Queue.Item
    , loadingState : AudioLoadingState
    , playbackPosition : Float
    }



-- 🌳  ░░  EVENTS


type alias DurationChangeEvent =
    { trackId : String, duration : Float }


type alias ErrorAudioEvent =
    { trackId : String, code : Int }


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
