module UI.Javascript.Task exposing (..)

import ConcurrentTask exposing (ConcurrentTask)
import UI.Ports as Ports
import UI.Types exposing (..)



-- 🛠️


attempt : ConcurrentTask Msg Msg -> Manager
attempt task model =
    let
        ( pool, cmd ) =
            ConcurrentTask.attempt
                { send = Ports.sendTask
                , pool = model.jsTasks
                , onComplete = JavascriptTaskCompleted
                }
                task
    in
    ( { model | jsTasks = pool }
    , cmd
    )
