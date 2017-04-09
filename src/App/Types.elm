module Types exposing (..)

import Date exposing (Date)
import Firebase.Auth
import Json.Encode
import Time exposing (Time)


-- Children

import Console.Types as Console
import Queue.Types as Queue
import Routing.Types as Routing
import Sources.Types as Sources


-- Types


type Msg
    = Authenticate
      -- Time
    | SetTimestamp Time
      -- Children
    | ConsoleMsg Console.Msg
    | QueueMsg Queue.Msg
    | RoutingMsg Routing.Msg
    | SourcesMsg Sources.Msg
      -- Children, Pt. 2
    | ProcessSources
      -- Other
    | NoOp


type alias Model =
    { authenticatedUser : Maybe Firebase.Auth.User
    , showLoadingScreen : Bool

    ------------------------------------
    -- Time
    ------------------------------------
    , timestamp : Date

    ------------------------------------
    -- Children
    ------------------------------------
    , console : Console.Model
    , queue : Queue.Model
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


type alias Illumination model childMsg =
    model -> List (Cmd childMsg) -> List (Cmd Msg) -> ( model, Cmd Msg )
