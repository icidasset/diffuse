port module UI.Ports exposing (activeQueueItemChanged, activeQueueItemEnded, fromBrain, giveBrain, nudgeBrain, pause, play, seek, setDuration, setIsLoading, setIsPlaying, setStalled, toBrain, toggleRepeat, unstall)

import Alien
import Json.Encode as Json
import Queue



-- 📣


port activeQueueItemChanged : Maybe Queue.EngineItem -> Cmd msg


port pause : () -> Cmd msg


port play : () -> Cmd msg


port seek : Float -> Cmd msg


port unstall : () -> Cmd msg


port toBrain : Alien.Event -> Cmd msg


port toggleRepeat : Bool -> Cmd msg



-- 📰


port activeQueueItemEnded : (() -> msg) -> Sub msg


port fromBrain : (Alien.Event -> msg) -> Sub msg


port setDuration : (Float -> msg) -> Sub msg


port setIsLoading : (Bool -> msg) -> Sub msg


port setIsPlaying : (Bool -> msg) -> Sub msg


port setStalled : (Bool -> msg) -> Sub msg



-- 🔱


giveBrain : Alien.Tag -> Json.Value -> Cmd msg
giveBrain tag data =
    toBrain (Alien.broadcast tag data)


nudgeBrain : Alien.Tag -> Cmd msg
nudgeBrain tag =
    toBrain (Alien.trigger tag)
