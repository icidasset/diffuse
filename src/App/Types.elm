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
import Tracks.Types as Tracks


-- Types


type Msg
    = Authenticate
    | HideLoadingScreen
    | SignOut
      -- Time
    | SetTimestamp Time
      -- Children
    | ConsoleMsg Console.Msg
    | QueueMsg Queue.Msg
    | RoutingMsg Routing.Msg
    | SourcesMsg Sources.Msg
    | TracksMsg Tracks.Msg
      -- Children, Pt. 2
    | ActiveQueueItemChanged (Maybe Queue.Item)
    | CleanQueue
    | FillQueue
    | RecalibrateTracks
    | ResetQueue
    | PlayTrack String
    | ProcessSources
    | ToggleFavourite String
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
    , tracks : Tracks.Model
    }


type alias Settings =
    { queue : Queue.Settings
    }


type alias ProgramFlags =
    { settings : Settings
    , user : Maybe Firebase.Auth.User

    -- Data
    , favourites : Maybe (List Json.Encode.Value)
    , sources : Maybe (List Json.Encode.Value)
    , tracks : Maybe (List Json.Encode.Value)
    }


type alias Illumination model childMsg =
    model -> List (Cmd childMsg) -> List (Cmd Msg) -> ( model, Cmd Msg )
