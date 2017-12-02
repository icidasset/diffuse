module Slave.Events exposing (..)

import Json.Encode as Encode
import Slave.Ports as Ports
import Slave.Types exposing (..)
import Utils


issue : AlienMsg -> Cmd Msg
issue msg =
    issueWithData msg Encode.null


issueWithData : AlienMsg -> Encode.Value -> Cmd Msg
issueWithData msg data =
    Ports.outgoing
        { tag = Utils.messageToString msg
        , data = data
        , error = Nothing
        }


reportError : String -> Cmd Msg
reportError err =
    Ports.outgoing
        { tag = Utils.messageToString ReportError
        , data = Encode.null
        , error = Just err
        }
