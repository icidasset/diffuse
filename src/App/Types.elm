module Types exposing (..)

import Firebase.Auth


-- Children

import Routing.Types as Routing


-- Types


type Msg
    = Authenticate
      -- Children
    | RoutingMsg Routing.Msg


type alias Model =
    { authenticatedUser : Maybe Firebase.Auth.User
    , showLoadingScreen : Bool
    , ------------------------------------
      -- Children
      ------------------------------------
      routing : Routing.Model
    }


type alias ProgramFlags =
    { user : Maybe Firebase.Auth.User }
