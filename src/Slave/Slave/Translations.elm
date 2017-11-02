module Slave.Translations exposing (..)

import Slave.Types exposing (AlienMsg)


-- Alien messages


stringToAlienMessage : String -> AlienMsg
stringToAlienMessage str =
    case str of
        "PROCESS_SOURCES" ->
            ProcessSources

        _ ->
            Debug.crash "Invalid AlienMsg"


alienMessageToString : AlienMsg -> String
alienMessageToString msg =
    case msg of
        ProcessSources ->
            "PROCESS_SOURCES"
