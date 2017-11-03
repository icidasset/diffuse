module Slave.Translations exposing (..)

import Slave.Types exposing (AlienMsg(..))


-- Alien messages


stringToAlienMessage : String -> AlienMsg
stringToAlienMessage str =
    case str of
        "ADD_TRACKS" ->
            AddTracks

        "PROCESS_SOURCES" ->
            ProcessSources

        "REMOVE_TRACKS_BY_PATH" ->
            RemoveTracksByPath

        _ ->
            Debug.crash "Invalid AlienMsg"


alienMessageToString : AlienMsg -> String
alienMessageToString msg =
    case msg of
        AddTracks ->
            "ADD_TRACKS"

        ProcessSources ->
            "PROCESS_SOURCES"

        RemoveTracksByPath ->
            "REMOVE_TRACKS_BY_PATH"
