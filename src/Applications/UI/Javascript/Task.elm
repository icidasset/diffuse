module UI.Javascript.Task exposing (..)

import ConcurrentTask
import UI.Ports as Ports
import UI.Types exposing (..)



-- 🛠️


attempt =
    ConcurrentTask.attempt
        { send = Ports.sendTask
        , pool = ConcurrentTask.pool
        , onComplete = JsTaskCompleted
        }
