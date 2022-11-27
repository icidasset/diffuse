module TaskPort.Extra exposing (..)

import TaskPort


errorToStringCustom : TaskPort.Error -> String
errorToStringCustom err =
    case err of
        TaskPort.JSError jsErr ->
            case jsErr of
                TaskPort.ErrorObject _ errRecord ->
                    errRecord.message

                TaskPort.ErrorValue _ ->
                    TaskPort.errorToString err

        TaskPort.InteropError _ ->
            TaskPort.errorToString err
