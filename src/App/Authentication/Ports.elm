port module Authentication.Ports exposing (..)

import Types exposing (AlienEvent)


-- 💡


port authenticationEvent : AlienEvent -> Cmd msg


port clearLocalUserData : () -> Cmd msg


port retrieveLocalUserData : () -> Cmd msg


port storeLocalUserData : String -> Cmd msg



-- 🚽


port authenticationEventResult : (AlienEvent -> msg) -> Sub msg


port retrievedLocalUserData : (Maybe String -> msg) -> Sub msg
