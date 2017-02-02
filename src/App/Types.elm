module Types exposing (..)

import Firebase.Auth


-- Children

import Routing.Types as Routing


-- Messages


type Msg
    = Authenticate
      -- Children
    | RoutingMsg Routing.Msg



-- Model


type alias Model =
    { authenticatedUser : Maybe Firebase.Auth.User
    , showLoadingScreen : Bool
    , ------------------------------------
      -- Children
      ------------------------------------
      routing : Routing.Model
    }



-- Flags


type alias ProgramFlags =
    { user : Maybe Firebase.Auth.User }
