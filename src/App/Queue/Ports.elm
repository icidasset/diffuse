port module Queue.Ports exposing (..)

import Queue.Types exposing (Item, Settings)


-- 💡


port activeQueueItemChanged : Maybe Item -> Cmd msg


port storeQueueSettings : Settings -> Cmd msg



-- 🚽


port activeQueueItemEnded : (() -> msg) -> Sub msg
