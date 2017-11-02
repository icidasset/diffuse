module Slave.Events exposing (..)

import Json.Encode as Encode
import Slave.Ports as Ports
import Slave.Translations as Translations
import Slave.Types exposing (..)
import Types as TopLevel


issue : AlienMsg -> Cmd TopLevel.Msg
issue msg =
    issueWithData msg Encode.null


issueWithData : AlienMsg -> Encode.Value -> Cmd TopLevel.Msg
issueWithData msg data =
    Ports.authenticationEvent
        { tag = Translations.alienMessageToString msg
        , data = data
        , error = Nothing
        }
