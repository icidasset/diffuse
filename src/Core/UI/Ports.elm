port module UI.Ports exposing (..)

import Alien
import Json.Encode as Json
import Queue
import UI.Audio.Types as Audio



-- 📣


port activeQueueItemChanged : Maybe Queue.EngineItem -> Cmd msg


port adjustEqualizerSetting : { knob : String, value : Float } -> Cmd msg


port copyToClipboard : String -> Cmd msg


port downloadJsonUsingTauri : { filename : String, json : String } -> Cmd msg


port loadAlbumCovers : { list : Bool, coverView : Bool } -> Cmd msg


port openUrlOnNewPage : String -> Cmd msg


port pause : { trackId : String } -> Cmd msg


port pauseScrobbleTimer : () -> Cmd msg


port pickAverageBackgroundColor : String -> Cmd msg


port play : { trackId : String, volume : Float } -> Cmd msg


port reloadAudioNodeIfNeeded : { play : Bool, progress : Maybe Float, trackId : String } -> Cmd msg


port preloadAudio : Queue.EngineItem -> Cmd msg


port reloadApp : () -> Cmd msg


port renderAudioElements : { items : List Queue.EngineItem, play : Maybe String, volume : Float } -> Cmd msg


port resetScrobbleTimer : { duration : Float, trackId : String } -> Cmd msg


port seek : { percentage : Float, trackId : String } -> Cmd msg


port sendTask : Json.Value -> Cmd msg


port setMediaSessionArtwork : { blobUrl : String, imageType : String } -> Cmd msg


port setMediaSessionMetadata : { album : Maybe String, artist : Maybe String, title : String, coverPrep : Maybe Audio.CoverPrep } -> Cmd msg


port setMediaSessionPlaybackState : String -> Cmd msg


port setMediaSessionPositionState : { currentTime : Float, duration : Float } -> Cmd msg


port startScrobbleTimer : () -> Cmd msg


port toBrain : Alien.Event -> Cmd msg



-- 📰


port audioDurationChange : (Audio.DurationChangeEvent -> msg) -> Sub msg


port audioEnded : (Audio.GenericAudioEvent -> msg) -> Sub msg


port audioError : (Audio.ErrorAudioEvent -> msg) -> Sub msg


port audioPlaybackStateChanged : (Audio.PlaybackStateEvent -> msg) -> Sub msg


port audioIsLoading : (Audio.GenericAudioEvent -> msg) -> Sub msg


port audioHasLoaded : (Audio.GenericAudioEvent -> msg) -> Sub msg


port audioTimeUpdated : (Audio.TimeUpdatedEvent -> msg) -> Sub msg


port downloadTracksFinished : (() -> msg) -> Sub msg


port fromAlien : (Alien.Event -> msg) -> Sub msg


port lostWindowFocus : (() -> msg) -> Sub msg


port indicateTouchDevice : (() -> msg) -> Sub msg


port insertCoverCache : (Json.Value -> msg) -> Sub msg


port installedNewServiceWorker : (() -> msg) -> Sub msg


port installingNewServiceWorker : (() -> msg) -> Sub msg


port refreshedAccessToken : (Json.Value -> msg) -> Sub msg


port preferredColorSchemaChanged : ({ dark : Bool } -> msg) -> Sub msg


port receiveTask : (Json.Value -> msg) -> Sub msg


port requestNext : (() -> msg) -> Sub msg


port requestPause : (() -> msg) -> Sub msg


port requestPlay : (() -> msg) -> Sub msg


port requestPlayPause : (() -> msg) -> Sub msg


port requestPrevious : (() -> msg) -> Sub msg


port requestStop : (() -> msg) -> Sub msg


port scrobble : ({ duration : Int, timestamp : Int, trackId : String } -> msg) -> Sub msg


port setAverageBackgroundColor : ({ r : Int, g : Int, b : Int } -> msg) -> Sub msg


port setIsOnline : (Bool -> msg) -> Sub msg


port showErrorNotification : (String -> msg) -> Sub msg


port showStickyErrorNotification : (String -> msg) -> Sub msg



-- 🔱


giveBrain : Alien.Tag -> Json.Value -> Cmd msg
giveBrain tag data =
    toBrain (Alien.broadcast tag data)


nudgeBrain : Alien.Tag -> Cmd msg
nudgeBrain tag =
    toBrain (Alien.trigger tag)
