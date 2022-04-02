module UI.Commands.State exposing (..)

import UI.Alfred.State as Alfred
import UI.Commands.Alfred
import UI.Types exposing (Manager)



-- 📣


showPalette : Manager
showPalette model =
    Alfred.assign
        (UI.Commands.Alfred.palette model)
        model
