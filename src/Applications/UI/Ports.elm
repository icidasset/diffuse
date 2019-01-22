port module UI.Ports exposing (fromBrain, giveBrain, nudgeBrain, toBrain)

import Alien
import Json.Encode as Json



-- 📣


port toBrain : Alien.Event -> Cmd msg



-- 📰


port fromBrain : (Alien.Event -> msg) -> Sub msg



-- 🔱


giveBrain : Alien.Tag -> Json.Value -> Cmd msg
giveBrain tag data =
    toBrain (Alien.broadcast tag data)


nudgeBrain : Alien.Tag -> Cmd msg
nudgeBrain tag =
    toBrain (Alien.trigger tag)
