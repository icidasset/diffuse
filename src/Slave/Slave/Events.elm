module Slave.Events exposing (..)

import Json.Encode as Encode
import Slave.Ports as Ports
import Slave.Translations as Translations
import Slave.Types exposing (..)


issue : AlienMsg -> Cmd Msg
issue msg =
    issueWithData msg Encode.null


issueWithData : AlienMsg -> Encode.Value -> Cmd Msg
issueWithData msg data =
    Ports.outgoing
        { tag = Translations.alienMessageToString msg
        , data = data
        , error = Nothing
        }


reportError : String -> Cmd Msg
reportError err =
    Ports.outgoing
        { tag = Translations.alienMessageToString ReportError
        , data = Encode.null
        , error = Just err
        }
