port module UI.Ports exposing (activeQueueItemChanged, activeQueueItemEnded, adjustEqualizerSetting, copyToClipboard, fromAlien, giveBrain, noteProgress, nudgeBrain, pause, pickAverageBackgroundColor, play, preloadAudio, requestNext, requestPlayPause, requestPrevious, requestStop, seek, setAudioDuration, setAudioHasStalled, setAudioIsLoading, setAudioIsPlaying, setAverageBackgroundColor, setIsOnline, setRepeat, showErrorNotification, toBrain)

import Alien
import Json.Encode as Json
import Queue



-- 📣


port activeQueueItemChanged : Maybe Queue.EngineItem -> Cmd msg


port adjustEqualizerSetting : { knob : String, value : Float } -> Cmd msg


port copyToClipboard : String -> Cmd msg


port pause : () -> Cmd msg


port pickAverageBackgroundColor : String -> Cmd msg


port play : () -> Cmd msg


port preloadAudio : Queue.EngineItem -> Cmd msg


port seek : Float -> Cmd msg


port setRepeat : Bool -> Cmd msg


port toBrain : Alien.Event -> Cmd msg



-- 📰


port activeQueueItemEnded : (() -> msg) -> Sub msg


port fromAlien : (Alien.Event -> msg) -> Sub msg


port noteProgress : (Float -> msg) -> Sub msg


port requestNext : (() -> msg) -> Sub msg


port requestPlayPause : (() -> msg) -> Sub msg


port requestPrevious : (() -> msg) -> Sub msg


port requestStop : (() -> msg) -> Sub msg


port showErrorNotification : (String -> msg) -> Sub msg


port setAudioDuration : (Float -> msg) -> Sub msg


port setAudioIsLoading : (Bool -> msg) -> Sub msg


port setAudioIsPlaying : (Bool -> msg) -> Sub msg


port setAudioHasStalled : (Bool -> msg) -> Sub msg


port setAverageBackgroundColor : ({ r : Int, g : Int, b : Int } -> msg) -> Sub msg


port setIsOnline : (Bool -> msg) -> Sub msg



-- 🔱


giveBrain : Alien.Tag -> Json.Value -> Cmd msg
giveBrain tag data =
    toBrain (Alien.broadcast tag data)


nudgeBrain : Alien.Tag -> Cmd msg
nudgeBrain tag =
    toBrain (Alien.trigger tag)
