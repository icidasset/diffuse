module Brain.Common.State exposing (..)

import Alien
import Brain.Ports as Ports
import Brain.Types exposing (..)
import Json.Decode as Json
import Return.Ext as Return
import Task
import TaskPort
import TaskPort.Extra as TaskPort



-- 🛠


attemptPortTask : (a -> Msg) -> Task.Task TaskPort.Error a -> Cmd Msg
attemptPortTask mapFn =
    Task.attempt (reportPortErrorToUI mapFn)


attemptTask : (a -> Msg) -> Task.Task String a -> Cmd Msg
attemptTask mapFn =
    Task.attempt (reportErrorToUI mapFn)



-- GIVE


giveUI : Alien.Tag -> Json.Value -> Manager
giveUI tag data =
    data
        |> giveUICmd tag
        |> Return.communicate


giveUICmd : Alien.Tag -> Json.Value -> Cmd Msg
giveUICmd tag data =
    data
        |> Alien.broadcast tag
        |> Ports.toUI


giveUICmdMsg : Alien.Tag -> Json.Value -> Msg
giveUICmdMsg tag data =
    data
        |> giveUICmd tag
        |> Cmd



-- NUDGE


nudgeUI : Alien.Tag -> Manager
nudgeUI =
    nudgeUICmd >> Return.communicate


nudgeUICmd : Alien.Tag -> Cmd Msg
nudgeUICmd tag =
    tag
        |> Alien.trigger
        |> Ports.toUI


nudgeUICmdMsg : Alien.Tag -> Msg
nudgeUICmdMsg =
    nudgeUICmd >> Cmd



-- REPORT


reportErrorToUI : (a -> Msg) -> Result String a -> Msg
reportErrorToUI mapFn result =
    case result of
        Ok value ->
            mapFn value

        Err error ->
            reportUICmdMsg Alien.ReportError error


reportPortErrorToUI : (a -> Msg) -> Result TaskPort.Error a -> Msg
reportPortErrorToUI mapFn =
    Result.mapError TaskPort.errorToStringCustom >> reportErrorToUI mapFn


reportUI : Alien.Tag -> String -> Manager
reportUI tag error =
    error
        |> reportUICmd tag
        |> Return.communicate


reportUICmd : Alien.Tag -> String -> Cmd Msg
reportUICmd tag error =
    error
        |> Alien.report tag
        |> Ports.toUI


reportUICmdMsg : Alien.Tag -> String -> Msg
reportUICmdMsg tag error =
    error
        |> reportUICmd tag
        |> Cmd
