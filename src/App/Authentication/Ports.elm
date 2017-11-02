port module Authentication.Ports exposing (..)

import Types exposing (AlienEvent)


-- 💡


port authenticationEvent : AlienEvent -> Cmd msg



-- 🚽


port authenticationEventResult : (AlienEvent -> msg) -> Sub msg
