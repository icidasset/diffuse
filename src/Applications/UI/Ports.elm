port module UI.Ports exposing (fromBrain, toBrain)

import Alien



-- 📣


port toBrain : Alien.Event -> Cmd msg



-- 📰


port fromBrain : (Alien.Event -> msg) -> Sub msg
