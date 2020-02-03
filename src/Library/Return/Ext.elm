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
    msg
        |> Task.succeed
        |> Task.perform identity
        |> Tuple.pair model
