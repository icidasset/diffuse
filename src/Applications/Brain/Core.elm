module Brain.Core exposing (Flags, Model, Msg(..))

import Alien
import Brain.Authentication as Authentication
import Brain.Sources.Processing.Common as Processing



-- ⛩


type alias Flags =
    {}



-- 🌳


type alias Model =
    { authentication : Authentication.Model
    , processing : Processing.Model
    }



-- 📣


type Msg
    = Bypass
    | NotifyUI Alien.Event
      -----------------------------------------
      -- Children
      -----------------------------------------
    | AuthenticationMsg Authentication.Msg
    | ProcessingMsg Processing.Msg
