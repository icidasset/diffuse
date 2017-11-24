module Types exposing (..)

import Date exposing (Date)
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Keyboard.Extra as Keyboard
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
    | DoAll (List Msg)
    | NoOp
    | Reset
    | SetIsTouchDevice Bool
      -- Alfred
    | CalculateAlfredResults String
    | HideAlfred
    | RequestAssistanceForPlaylists (List Tracks.Track)
    | RunAlfredAction Int
      -- Context Menu
    | HideContextMenu
    | ShowSourceMenu String Mouse.Position
    | ShowTrackContextMenu ( String, Mouse.Position )
      -- Keyboard
    | KeydownMsg Keyboard.Key
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
    | StoreUserData
    | DebounceStoreUserData
    | DebounceCallbackStoreUserData Debounce.Msg
      --
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
      --
      -- Children, Pt. 2
    | ActiveQueueItemChanged (Maybe Queue.Item)
    | AutoGeneratePlaylists
    | CheckSelectedPlaylist
    | FillQueue
    | PlayTrack String
    | ProcessSources
      --
      -- Slave events
    | Extraterrestrial Slave.Types.AlienMsg Slave.Types.AlienResult



-- Model


type alias Model =
    { alfred : Maybe Alfred
    , contextMenu : Maybe ContextMenu
    , isDevelopmentEnvironment : Bool
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



-- Alfred


type alias Alfred =
    { action : Maybe String -> Maybe String -> Cmd Msg
    , focus : Int
    , index : List String
    , message : String
    , results : List String
    , searchTerm : Maybe String
    }



-- Other


type alias AlienEvent =
    { tag : String, data : Json.Encode.Value, error : Maybe String }


type alias Illumination model childMsg =
    model -> List (Cmd childMsg) -> List (Cmd Msg) -> ( model, Cmd Msg )


type alias ProgramFlags =
    { isDevelopmentEnvironment : Bool, isHTTPS : Bool }
