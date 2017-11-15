port module Ports exposing (..)

import Types exposing (AlienEvent)


-- 💡


port fadeInNotifications : () -> Cmd msg


port slaveEvent : AlienEvent -> Cmd msg



-- 🚽


port setIsTouchDevice : (Bool -> msg) -> Sub msg


port slaveEventResult : (AlienEvent -> msg) -> Sub msg
