module Types exposing (..)

import Date exposing (Date)
import Firebase.Auth
import Json.Encode
import Time exposing (Time)


-- Children

import Queue.Types as Queue
import Routing.Types as Routing
import Sources.Types as Sources
import Tracks.Types as Tracks


-- Types


type Msg
    = Authenticate
      -- Time
    | SetTimestamp Time
      -- Children
    | QueueMsg Queue.Msg
    | RoutingMsg Routing.Msg
    | SourcesMsg Sources.Msg


type alias Model =
    { authenticatedUser : Maybe Firebase.Auth.User
    , showLoadingScreen : Bool
    , ------------------------------------
      -- Time
      ------------------------------------
      timestamp : Date
    , ------------------------------------
      -- Children
      ------------------------------------
      queue : Queue.Model
    , routing : Routing.Model
    , sources : Sources.Model
    }


type alias Settings =
    { queue : Queue.Settings
    }


type alias ProgramFlags =
    { settings : Settings
    , sources : Maybe (List Json.Encode.Value)
    , tracks : Maybe (List Json.Encode.Value)
    , user : Maybe Firebase.Auth.User
    }
