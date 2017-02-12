port module Queue.Ports exposing (..)

import Queue.Types exposing (Item)


-- 💡


port activeQueueItemChanged : Maybe Item -> Cmd msg



-- 🚽


port activeQueueItemEnded : (() -> msg) -> Sub msg
