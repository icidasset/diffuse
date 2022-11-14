module Task.Extra exposing (do, doDelayed, fromResult)

import Process
import Task



-- 🔱


do : msg -> Cmd msg
do msg =
    Task.perform identity (Task.succeed msg)


doDelayed : Float -> msg -> Cmd msg
doDelayed delay msg =
    Task.perform (always msg) (Process.sleep delay)


fromResult : Result error value -> Task.Task error value
fromResult result =
    case result of
        Ok v ->
            Task.succeed v

        Err e ->
            Task.fail e
