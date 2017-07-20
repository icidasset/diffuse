module Types exposing (..)

import Date exposing (Date)
import Json.Encode
import Mouse
import Svg exposing (Svg)
import Time exposing (Time)
import Window


-- Children

import Console.Types as Console
import Queue.Types as Queue
import Routing.Types as Routing
import Sources.Types as Sources
import Tracks.Types as Tracks


-- Messages


type Msg
    = Authenticate AuthMethod
    | ClickAway
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
    | ShowTrackContextMenu ( String, Mouse.Position )
    | ShowViewMenu
    | ShowViewMenuWithWindow Window.Size
    | ToggleFavourite String
      -- Other
    | NoOp



-- Model


type alias Model =
    { authenticatedUser : Maybe User
    , contextMenu : Maybe ContextMenu
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



-- Flags


type alias ProgramFlags =
    { settings : Settings
    , user : Maybe User

    -- Data
    , favourites : Maybe (List Json.Encode.Value)
    , sources : Maybe (List Json.Encode.Value)
    , tracks : Maybe (List Json.Encode.Value)
    }



-- Settings


type alias Settings =
    { queue : Queue.Settings
    , tracks : Tracks.Settings
    }



-- Context Menu


type ContextMenu
    = ContextMenu ContextMenuItems Mouse.Position


type alias ContextMenuItems =
    List ( Svg Msg, String, Msg )



-- Other


type AuthMethod
    = Local
    | Blockstack


type alias User =
    { displayName : String
    }


type alias Illumination model childMsg =
    model -> List (Cmd childMsg) -> List (Cmd Msg) -> ( model, Cmd Msg )
