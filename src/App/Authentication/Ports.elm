port module Authentication.Ports exposing (..)

import Authentication.Types exposing (..)


-- 💡


port authenticationEvent : OutsideEvent -> Cmd msg



-- 🚽


port authenticationEventResult : (OutsideEvent -> msg) -> Sub msg
