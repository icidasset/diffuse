module Brain.Core exposing (Flags, Model, Msg(..))

import Alien
import Brain.Authentication



-- ⛩


type alias Flags =
    {}



-- 🌳


type alias Model =
    { authentication : Brain.Authentication.Model }



-- 📣


type Msg
    = Bypass
    | NotifyUI Alien.Event
      -----------------------------------------
      -- Children
      -----------------------------------------
    | AuthenticationMsg Brain.Authentication.Msg
