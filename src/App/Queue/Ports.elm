port module Queue.Ports exposing (..)

import Queue.Types exposing (EngineItem, Settings)


-- 💡


port activeQueueItemChanged : Maybe EngineItem -> Cmd msg


port toggleRepeat : Bool -> Cmd msg



-- 🚽


port activeQueueItemEnded : (() -> msg) -> Sub msg
