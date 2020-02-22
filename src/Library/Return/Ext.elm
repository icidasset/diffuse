module Return.Ext exposing (..)

import Task



-- 🔱


communicate : Cmd msg -> model -> ( model, Cmd msg )
communicate c m =
    ( m, c )


{-| TODO: Remove when finished with refactor
-}
performance : msg -> model -> ( model, Cmd msg )
performance msg model =
    ( model, task msg )


performanceF : model -> msg -> ( model, Cmd msg )
performanceF model msg =
    performance msg model


task : msg -> Cmd msg
task msg =
    msg
        |> Task.succeed
        |> Task.perform identity
