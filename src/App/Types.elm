module Types exposing (..)

import Date exposing (Date)
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Json.Encode
import Mouse
import Notifications.Types exposing (Notification)
import Slave.Types
import Svg exposing (Svg)
import Toasty
import Time exposing (Time)
import Window


-- Children

import Abroad.Types as Abroad
import Authentication.Types as Authentication
import Console.Types as Console
import Equalizer.Types as Equalizer
import Playlists.Types as Playlists
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
    | ImportUserData String
    | StoreUserData
    | DebounceStoreUserData
    | DebounceCallbackStoreUserData Debounce.Msg
      -- Time
    | SetTimestamp Time
      -- Children
    | AbroadMsg Abroad.Msg
    | AuthenticationMsg Authentication.Msg
    | ConsoleMsg Console.Msg
    | EqualizerMsg Equalizer.Msg
    | PlaylistsMsg Playlists.Msg
    | QueueMsg Queue.Msg
    | RoutingMsg Routing.Msg
    | SettingsMsg Settings.Msg
    | SourcesMsg Sources.Msg
    | TracksMsg Tracks.Msg
      -- Children, Pt. 2
    | ActiveQueueItemChanged (Maybe Queue.Item)
    | AutoGeneratePlaylists
    | FillQueue
    | PlayTrack String
    | ProcessSources
      -- Slave events
    | Extraterrestrial Slave.Types.AlienMsg Slave.Types.AlienResult
      -- Notifications
    | ShowNotification Notification
      -- Context Menu
    | ShowSourceMenu String Mouse.Position
    | ShowTrackContextMenu ( String, Mouse.Position )
      -- Libraries
    | ToastyMsg (Toasty.Msg Notification)
      -- Other
    | NoOp



-- Model


type alias Model =
    { contextMenu : Maybe ContextMenu
    , isHTTPS : Bool
    , isTouchDevice : Bool
    , showLoadingScreen : Bool
    , toasties : Toasty.Stack Notification

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
    , playlists : Playlists.Model
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


type alias AlienEvent =
    { tag : String, data : Json.Encode.Value, error : Maybe String }


type alias Illumination model childMsg =
    model -> List (Cmd childMsg) -> List (Cmd Msg) -> ( model, Cmd Msg )


type alias ProgramFlags =
    { isHTTPS : Bool }
