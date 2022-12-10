module UI.User.State exposing (..)

import Return
import UI.Syncing.State as Syncing
import UI.Types exposing (..)
import User.Layer as User
import Webnative exposing (Artifact(..), DecodedResponse(..))
import Webnative.Tag as Tag



-- 🔱


gotWebnativeResponse : Webnative.Response -> Manager
gotWebnativeResponse response model =
    case Webnative.decodeResponse Tag.fromString response of
        Webnative (Initialisation state) ->
            if Webnative.isAuthenticated state then
                Syncing.activateSync
                    (User.Fission { initialised = False })
                    model

            else
                Return.singleton model

        _ ->
            Return.singleton model
