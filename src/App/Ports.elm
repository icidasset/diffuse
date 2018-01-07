port module Ports exposing (..)

import Types exposing (AlienEvent)


-- 💡


port fadeInNotifications : () -> Cmd msg


port slaveEvent : AlienEvent -> Cmd msg



-- 🚽


port setIsTouchDevice : (Bool -> msg) -> Sub msg


port shortcutNext : (() -> msg) -> Sub msg


port shortcutPlayPause : (() -> msg) -> Sub msg


port shortcutPrevious : (() -> msg) -> Sub msg


port slaveEventResult : (AlienEvent -> msg) -> Sub msg
