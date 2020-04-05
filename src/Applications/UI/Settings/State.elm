module UI.Settings.State exposing (..)

import Alien
import Return exposing (return)
import Settings exposing (Settings)
import UI.Ports as Ports
import UI.Types exposing (..)



-- 🔱
-- TODO: Move to User.State.Export


gatherSettings : Model -> Settings
gatherSettings { chosenBackdrop, hideDuplicates, lastFm, processAutomatically, rememberProgress } =
    { backgroundImage = chosenBackdrop
    , hideDuplicates = hideDuplicates
    , lastFm = lastFm.sessionKey
    , processAutomatically = processAutomatically
    , rememberProgress = rememberProgress
    }


save : Manager
save model =
    model
        |> gatherSettings
        |> Settings.encode
        |> Alien.broadcast Alien.SaveSettings
        |> Ports.toBrain
        |> return model
