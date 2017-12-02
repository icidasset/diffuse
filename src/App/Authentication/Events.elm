module Authentication.Events exposing (..)

import Authentication.Ports as Ports
import Authentication.Types exposing (..)
import Json.Encode as Encode
import Types as TopLevel
import Utils


issue : AlienMsg -> Cmd TopLevel.Msg
issue msg =
    issueWithData msg Encode.null


issueWithData : AlienMsg -> Encode.Value -> Cmd TopLevel.Msg
issueWithData msg data =
    Ports.authenticationEvent
        { tag = Utils.messageToString msg
        , data = data
        , error = Nothing
        }
