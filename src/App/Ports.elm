port module Ports exposing (..)

{-| In & Out.

💡 Send a message to the javascript world.
🚽 Receive a message from the javascript world.

-}

import Json.Encode
import Types exposing (AlienEvent)


-- 💡


port fadeInNotifications : () -> Cmd msg


port slaveEvent : AlienEvent -> Cmd msg



-- 🚽


port setIsTouchDevice : (Bool -> msg) -> Sub msg


port setIsOnline : (Bool -> msg) -> Sub msg


port shortcutNext : (() -> msg) -> Sub msg


port shortcutPlayPause : (() -> msg) -> Sub msg


port shortcutPrevious : (() -> msg) -> Sub msg


port slaveEventResult : (AlienEvent -> msg) -> Sub msg


port syncCompleted : (Json.Encode.Value -> msg) -> Sub msg


port syncStarted : (() -> msg) -> Sub msg
