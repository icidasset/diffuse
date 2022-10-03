module UI.User.State exposing (..)

import Return
import UI.Authentication.State as Authentication
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


syncData : Manager
syncData model =
    -- TODO:
    -- 1. Check if any existing data is present on the service to sync with.
    -- 2. If not, copy over all current data (in memory) to that service.
    --    If so: 👇
    -- 3. If no data is present locally then just load the remote data (ie. service data)
    --    No data = no sources, favourites & playlists
    --    If so: 👇
    -- 4. Ask the user if the data should be merged.
    --    If not: Load remote data
    --    If so: 👇
    -- 5. Load remote data and run merge function for each type of data (sources, tracks, etc.)
    -- 6. Store merged data into memory
    -- 7. Overwrite remote data
    Return.singleton model
