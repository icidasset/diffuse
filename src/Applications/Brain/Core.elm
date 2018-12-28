module Brain.Core exposing (Flags, Model, Msg(..))

import Alien
import Brain.Authentication
import Brain.Sources.Processing



-- ⛩


type alias Flags =
    {}



-- 🌳


type alias Model =
    { authentication : Brain.Authentication.Model
    , sourceProcessing : Brain.Sources.Processing.Model
    }



-- 📣


type Msg
    = Bypass
    | NotifyUI Alien.Event
      -----------------------------------------
      -- Children
      -----------------------------------------
    | AuthenticationMsg Brain.Authentication.Msg
    | SourceProcessingMsg Brain.Sources.Processing.Msg
