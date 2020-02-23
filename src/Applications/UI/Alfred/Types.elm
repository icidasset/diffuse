module UI.Alfred.Types exposing (..)

import Alfred exposing (Alfred)
import Keyboard
import UI.Reply exposing (Reply)



-- 📣


type Msg action
    = Assign (Alfred action)
    | DetermineResults String
    | RunAction Int
      -----------------------------------------
      -- Keyboard
      -----------------------------------------
    | KeyDown (Maybe Keyboard.Key)
