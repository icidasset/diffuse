module Brain.Core exposing (Flags, Model, Msg(..))

import Alien
import Authentication
import Brain.Authentication as Authentication
import Brain.Sources.Processing.Common as Processing
import Brain.Tracks as Tracks
import Json.Decode as Json



-- ⛩


type alias Flags =
    {}



-- 🌳


type alias Model =
    { authentication : Authentication.Model
    , hypaethralUserData : Authentication.HypaethralUserData
    , processing : Processing.Model
    , tracks : Tracks.Model
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
    | TracksMsg Tracks.Msg
      -----------------------------------------
      -- User data
      -----------------------------------------
    | LoadHypaethralUserData Json.Value
    | SaveFavourites Json.Value
    | SaveSources Json.Value
    | SaveTracks Json.Value
