module Types exposing (..)

import Date exposing (Date)
import Debounce exposing (Debounce)
import Element exposing (Element)
import Html
import Json.Encode
import Keyboard.Extra as Keyboard
import Lazy exposing (Lazy)
import Mouse
import Notifications.Types exposing (Notification)
import Slave.Types
import Toasty
import Time exposing (Time)


-- Children

import Abroad.Types as Abroad
import Alfred.Types as Alfred
import Authentication.Types as Authentication
import Console.Types as Console
import ContextMenu.Types as ContextMenu
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
    | NoOp
    | Reset
    | SetIsTouchDevice Bool
    | SetIsOnline Bool
      -- Keyboard
    | KeydownMsg Keyboard.Key
    | KeyupMsg Keyboard.Key
      -- Libraries
    | ToastyMsg (Toasty.Msg Notification)
      -- Loading
    | HideLoadingScreen
    | ShowLoadingScreen
      -- Notifications
    | ShowNotification Notification
      -- Time
    | SetTimestamp Time
      -- User layer
    | ImportUserData String
    | ImportLocalUserData String
    | InsertDemoContent String
    | StoreUserData
    | StoreLocalUserData
    | SyncCompleted Json.Encode.Value
    | SyncStarted
    | DebounceStoreUserData
    | DebounceCallbackStoreUserData Debounce.Msg
    | DebounceStoreLocalUserData
    | DebounceCallbackStoreLocalUserData Debounce.Msg
      --
      -- Children
    | AbroadMsg Abroad.Msg
    | AlfredMsg (Alfred.Msg Msg)
    | AuthenticationMsg Authentication.Msg
    | ConsoleMsg Console.Msg
    | ContextMenuMsg ContextMenu.Msg
    | EqualizerMsg Equalizer.Msg
    | PlaylistsMsg Playlists.Msg
    | QueueMsg Queue.Msg
    | RoutingMsg Routing.Msg
    | SettingsMsg Settings.Msg
    | SourcesMsg Sources.Msg
    | TracksMsg Tracks.Msg
      --
      -- Children, Pt. 2
    | ApplyTrackSelection String
    | ActiveQueueItemChanged (Maybe Queue.Item)
    | AutoGeneratePlaylists
    | CheckSelectedPlaylist
    | FillQueue
    | PlayTrack String
    | ProcessSources
    | RequestAssistanceForPlaylists (List Tracks.Track)
    | ShowTrackContextMenu ( String, Mouse.Position )
    | SetEnabledSourceIds (List Sources.Source)
      --
      -- Slave events
    | Extraterrestrial Slave.Types.AlienMsg Slave.Types.AlienResult



-- Model


type alias Model =
    { holdingShiftKey : Bool
    , isDevelopmentEnvironment : Bool
    , isElectron : Bool
    , isHTTPS : Bool
    , isOnline : Bool
    , isTouchDevice : Bool
    , origin : String
    , screenHeight : Int
    , showLoadingScreen : Bool
    , toasties : Toasty.Stack Notification

    ------------------------------------
    -- Time
    ------------------------------------
    , ludStorageDebounce : Debounce ()
    , udStorageDebounce : Debounce ()
    , timestamp : Date

    ------------------------------------
    -- Children
    ------------------------------------
    , abroad : Abroad.Model
    , alfred : Alfred.Model Msg
    , authentication : Authentication.Model
    , console : Console.Model
    , contextMenu : ContextMenu.Model Msg
    , equalizer : Equalizer.Model
    , playlists : Playlists.Model
    , queue : Queue.Model
    , routing : Routing.Model
    , settings : Settings.Model
    , sources : Sources.Model
    , tracks : Tracks.Model
    }



-- Other


type alias AlienEvent =
    { tag : String, data : Json.Encode.Value, error : Maybe String }


type alias Illumination model childMsg =
    model -> List (Cmd childMsg) -> List (Cmd Msg) -> ( model, Cmd Msg )


type alias ProgramFlags =
    { isDevelopmentEnvironment : Bool
    , isElectron : Bool
    , isHTTPS : Bool
    , isOnline : Bool
    , screenHeight : Int
    }
