module Slave.Translations exposing (..)

import Slave.Types exposing (AlienMsg)


-- Alien messages


stringToAlienMessage : String -> AlienMsg
stringToAlienMessage str =
    case str of
        _ ->
            Debug.crash "Invalid AlienMsg"


alienMessageToString : AlienMsg -> String
alienMessageToString msg =
    toString msg
