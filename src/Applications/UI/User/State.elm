module UI.User.State exposing (..)

import Alien
import Return exposing (return)
import UI.Authentication.State as Authentication
import UI.Ports as Ports
import UI.Types as UI exposing (..)
import User.Layer as User
import Webnative exposing (Artifact(..), DecodedResponse(..))
import Webnative.Tag as Tag



-- 🔱


gotWebnativeResponse : Webnative.Response -> Manager
gotWebnativeResponse response model =
    case Webnative.decodeResponse Tag.fromString response of
        Webnative (Initialisation state) ->
            if Webnative.isAuthenticated state then
                Authentication.signIn
                    (User.Fission { initialised = False })
                    model

            else
                Return.singleton model

        _ ->
            Return.singleton model


migrateHypaethralUserData : Manager
migrateHypaethralUserData model =
    Authentication.signOut { model | migratingData = True }
