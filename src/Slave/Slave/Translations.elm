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

        "REPORT_PROCESSING_ERROR" ->
            ReportProcessingError

        "UPDATE_SOURCE_DATA" ->
            UpdateSourceData

        _ ->
            Debug.crash "Invalid AlienMsg"
