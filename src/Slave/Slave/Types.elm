module Slave.Types exposing (..)

-- Messages


type Msg
    = Extraterrestrial AlienMsg AlienResult



-- Talking to the outside world


type AlienMsg
    = ProcessSources


type alias AlienResult =
    Result String Json.Encode.Value
