module Types exposing (..)

import Date exposing (Date)
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Json.Encode
import Mouse
import Svg exposing (Svg)
import Time exposing (Time)
import Window


-- Children

import Abroad.Types as Abroad
import Authentication.Types as Authentication
import Console.Types as Console
import Equalizer.Types as Equalizer
import Queue.Types as Queue
import Routing.Types as Routing
import Settings.Types as Settings
import Sources.Types as Sources
import Tracks.Types as Tracks


-- Messages


type Msg
    = ClickAway
    | Reset
    | SetIsTouchDevice Bool
      -- Loading
    | HideLoadingScreen
    | ShowLoadingScreen
      -- User layer
    | DidStoreUserData (Result String ())
    | ImportUserData String
    | StoreUserData
      -- Time
    | DebounceStoreUserData
    | DebounceCallbackStoreUserData Debounce.Msg
    | SetTimestamp Time
      -- Children
    | AbroadMsg Abroad.Msg
    | AuthenticationMsg Authentication.Msg
    | ConsoleMsg Console.Msg
    | EqualizerMsg Equalizer.Msg
    | QueueMsg Queue.Msg
    | RoutingMsg Routing.Msg
    | SettingsMsg Settings.Msg
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
      -- Children, Pt. 3
    | ShowSourceMenu String Mouse.Position
    | ShowTrackContextMenu ( String, Mouse.Position )
    | ShowViewMenu
    | ShowViewMenuWithWindow Window.Size
      -- Other
    | NoOp



-- Model


type alias Model =
    { contextMenu : Maybe ContextMenu
    , isTouchDevice : Bool
    , showLoadingScreen : Bool

    ------------------------------------
    -- Time
    ------------------------------------
    , storageDebounce : Debounce ()
    , timestamp : Date

    ------------------------------------
    -- Children
    ------------------------------------
    , abroad : Abroad.Model
    , authentication : Authentication.Model
    , console : Console.Model
    , equalizer : Equalizer.Model
    , queue : Queue.Model
    , routing : Routing.Model
    , settings : Settings.Model
    , sources : Sources.Model
    , tracks : Tracks.Model
    }



-- Context Menu


type ContextMenu
    = ContextMenu ContextMenuItems Mouse.Position


type alias ContextMenuItems =
    List ( Svg Msg, String, Msg )



-- Other


type alias Illumination model childMsg =
    model -> List (Cmd childMsg) -> List (Cmd Msg) -> ( model, Cmd Msg )
