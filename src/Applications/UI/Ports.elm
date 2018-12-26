port module UI.Ports exposing (fromBrain, removeFocus, toBrain)

import Alien



-- 📣


port removeFocus : () -> Cmd msg


port toBrain : Alien.Event -> Cmd msg



-- 📰


port fromBrain : (Alien.Event -> msg) -> Sub msg
