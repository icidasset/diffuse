port module Slave.Ports exposing (..)

import Types exposing (AlienEvent)


-- 💡


port outgoing : AlienEvent -> Cmd msg



-- 🚽


port incoming : (AlienEvent -> msg) -> Sub msg
