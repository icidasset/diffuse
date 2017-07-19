module Response.Ext exposing (..)

import Process
import Task
import Time exposing (Time)


addCmd : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCmd cmdToAdd ( model, currentCmd ) =
    ( model
    , Cmd.batch [ currentCmd, cmdToAdd ]
    )


andAlso : (model -> Cmd msg) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andAlso fnForCmd ( model, cmd ) =
    ( model
    , Cmd.batch
        [ cmd
        , fnForCmd model
        ]
    )


withAlso : (model -> Cmd msg) -> model -> ( model, Cmd msg )
withAlso fnForCmd model =
    ( model
    , fnForCmd model
    )



-- Tasks


do : msg -> Cmd msg
do msg =
    Task.perform identity (Task.succeed msg)


doDelayed : Time -> msg -> Cmd msg
doDelayed delay msg =
    Task.perform (always msg) (Process.sleep delay)
