module UI.User.State exposing (..)

import UI.Authentication.State as Authentication
import UI.Types as UI exposing (..)



-- 🔱


migrateHypaethralUserData : Manager
migrateHypaethralUserData model =
    Authentication.signOut { model | migratingData = True }
