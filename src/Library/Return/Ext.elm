module Return.Ext exposing (..)

import Task



-- 🔱


communicate : Cmd msg -> model -> ( model, Cmd msg )
communicate c m =
    ( m, c )


task : msg -> Cmd msg
task msg =
    msg
        |> Task.succeed
        |> Task.perform identity
