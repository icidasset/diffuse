module Task.Extra exposing (do)

import Task



-- 🔱


do : msg -> Cmd msg
do msg =
    Task.perform identity (Task.succeed msg)
