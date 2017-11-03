module Authentication.Events exposing (..)

import Authentication.Ports as Ports
import Authentication.Translations as Translations
import Authentication.Types exposing (..)
import Json.Encode as Encode
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
