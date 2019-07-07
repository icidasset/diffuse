module Task.Extra exposing (do, doDelayed)

import Process
import Task



-- 🔱


do : msg -> Cmd msg
do msg =
    Task.perform identity (Task.succeed msg)


doDelayed : Float -> msg -> Cmd msg
doDelayed delay msg =
    Task.perform (always msg) (Process.sleep delay)
