module Types exposing (..)

import Firebase.Auth


-- Children

import Queue.Types as Queue
import Routing.Types as Routing


-- Types


type Msg
    = Authenticate
      -- Children
    | QueueMsg Queue.Msg
    | RoutingMsg Routing.Msg


type alias Model =
    { authenticatedUser : Maybe Firebase.Auth.User
    , showLoadingScreen : Bool
    , ------------------------------------
      -- Children
      ------------------------------------
      queue : Queue.Model
    , routing : Routing.Model
    }


type alias Settings =
    { queue : Queue.Settings
    }


type alias ProgramFlags =
    { settings : Settings
    , user : Maybe Firebase.Auth.User
    }
