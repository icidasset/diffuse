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

        "PROCESS_SOURCES_COMPLETED" ->
            ProcessSourcesCompleted

        "REMOVE_TRACKS_BY_PATH" ->
            RemoveTracksByPath

        "REPORT_ERROR" ->
            ReportError

        _ ->
            Debug.crash "Invalid AlienMsg"


alienMessageToString : AlienMsg -> String
alienMessageToString msg =
    case msg of
        AddTracks ->
            "ADD_TRACKS"

        ProcessSources ->
            "PROCESS_SOURCES"

        ProcessSourcesCompleted ->
            "PROCESS_SOURCES_COMPLETED"

        RemoveTracksByPath ->
            "REMOVE_TRACKS_BY_PATH"

        ReportError ->
            "REPORT_ERROR"
