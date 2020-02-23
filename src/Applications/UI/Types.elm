module UI.Types exposing (..)

import Alfred exposing (Alfred)
import Browser
import Browser.Navigation as Nav
import Chunky exposing (..)
import Common exposing (Switch)
import Conditional exposing (..)
import ContextMenu exposing (ContextMenu)
import Coordinates exposing (Viewport)
import Css exposing (url)
import Css.Classes as C
import Debouncer.Basic as Debouncer exposing (Debouncer)
import Dict exposing (Dict)
import Dict.Ext as Dict
import Equalizer exposing (Knob)
import File exposing (File)
import Html exposing (Html, section)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (on, onClick)
import Html.Events.Extra.Pointer as Pointer
import Html.Lazy as Lazy
import Http
import Json.Decode
import Keyboard
import LastFm
import List.Ext as List
import List.Extra as List
import Management
import Maybe.Extra as Maybe
import Notifications exposing (Notification)
import Playlists exposing (PlaylistTrack)
import Playlists.Encoding as Playlists
import Queue
import Return2 exposing (..)
import Sources
import Sources.Encoding as Sources
import String.Ext as String
import Time
import Tracks
import Tracks.Encoding as Tracks
import UI.Alfred.Types as Alfred
import UI.Audio.Types as Audio
import UI.Authentication as Authentication
import UI.Authentication.ContextMenu as Authentication
import UI.Backdrop as Backdrop
import UI.Demo as Demo
import UI.DnD as DnD
import UI.Equalizer.Types as Equalizer
import UI.Navigation as Navigation
import UI.Notifications
import UI.Page as Page exposing (Page)
import UI.Playlists as Playlists
import UI.Playlists.ContextMenu as Playlists
import UI.Ports as Ports
import UI.Queue as Queue
import UI.Queue.ContextMenu as Queue
import UI.Reply as Reply exposing (Reply(..))
import UI.Settings as Settings
import UI.Sources as Sources
import UI.Sources.ContextMenu as Sources
import UI.Tracks as Tracks
import UI.Tracks.ContextMenu as Tracks
import Url exposing (Protocol(..), Url)
import Url.Ext as Url
import User.Layer exposing (..)
import User.Layer.Methods.RemoteStorage as RemoteStorage



-- ⛩


type alias Flags =
    { darkMode : Bool
    , initialTime : Int
    , isOnline : Bool
    , upgrade : Bool
    , viewport : Viewport
    }



-- 🌳


type alias Model =
    { alfred : Maybe (Alfred Msg)
    , contextMenu : Maybe (ContextMenu Reply)
    , confirmation : Maybe String
    , currentTime : Time.Posix
    , darkMode : Bool
    , debounce : Debouncer Msg Msg
    , downloading : Maybe { notificationId : Int }
    , focusedOnInput : Bool
    , isDragging : Bool
    , isLoading : Bool
    , isOnline : Bool
    , isTouchDevice : Bool
    , isUpgrading : Bool
    , lastFm : LastFm.Model
    , navKey : Nav.Key
    , notifications : UI.Notifications.Model
    , page : Page
    , pressedKeys : List Keyboard.Key
    , processAutomatically : Bool
    , url : Url
    , viewport : Viewport

    -----------------------------------------
    -- Children
    -----------------------------------------
    , authentication : Authentication.Model
    , backdrop : Backdrop.Model
    , equalizer : Equalizer.Model
    , queue : Queue.Model
    , playlists : Playlists.Model
    , sources : Sources.Model
    , tracks : Tracks.Model

    -----------------------------------------
    -- Pieces
    -----------------------------------------
    , audio : Audio.Model
    }



-- 📣


type Msg
    = Bypass
    | Reply Reply
      -----------------------------------------
      -- Authentication
      -----------------------------------------
    | AuthenticationBootFailure String
    | MissingSecretKey Json.Decode.Value
    | NotAuthenticated
    | RemoteStorageWebfinger RemoteStorage.Attributes (Result Http.Error String)
      -----------------------------------------
      -- Children (TODO)
      -----------------------------------------
    | AuthenticationMsg Authentication.Msg
    | BackdropMsg Backdrop.Msg
    | PlaylistsMsg Playlists.Msg
    | QueueMsg Queue.Msg
    | SourcesMsg Sources.Msg
    | TracksMsg Tracks.Msg
      -----------------------------------------
      -- Equalizer
      -----------------------------------------
    | ActivateKnob Knob Pointer.Event
    | AdjustKnob Pointer.Event
    | DeactivateKnob Pointer.Event
    | ResetKnob Knob
      -----------------------------------------
      -- Interface
      -----------------------------------------
    | Blur
    | Debounce (Debouncer.Msg Msg)
    | FocusedOnInput
    | HideOverlay
    | KeyboardMsg Keyboard.Msg
    | PreferredColorSchemaChanged { dark : Bool }
    | RemoveQueueSelection
    | RemoveTrackSelection
    | ResizedWindow ( Int, Int )
    | ShowNotification (Notification Reply)
    | SetIsTouchDevice Bool
    | StoppedDragging
    | ToggleLoadingScreen Switch
      -----------------------------------------
      -- Playlists
      -----------------------------------------
    | AddTracksToPlaylist { playlistName : String, tracks : List PlaylistTrack }
      -----------------------------------------
      -- Routing
      -----------------------------------------
    | ChangeUrlUsingPage Page
    | LinkClicked Browser.UrlRequest
    | PageChanged Page
    | UrlChanged Url
      -----------------------------------------
      -- Services
      -----------------------------------------
    | GotLastFmSession (Result Http.Error String)
    | Scrobble { duration : Int, timestamp : Int, trackId : String }
      -----------------------------------------
      -- Tracks
      -----------------------------------------
    | DownloadTracksFinished
    | FailedToStoreTracksInCache (List String)
    | FinishedStoringTracksInCache (List String)
      -----------------------------------------
      -- User
      -----------------------------------------
    | ImportFile File
    | ImportJson String
    | LoadEnclosedUserData Json.Decode.Value
    | LoadHypaethralUserData Json.Decode.Value
    | SaveEnclosedUserData
      -----------------------------------------
      -- 📭 Et Cetera
      -----------------------------------------
    | SetCurrentTime Time.Posix
    | SetIsOnline Bool
      -----------------------------------------
      -- 🦉 Nested
      -----------------------------------------
    | Alfred (Alfred.Msg Msg)
    | Audio Audio.Msg


type alias Organizer model =
    Management.Manager Msg model


type alias Manager =
    Organizer Model
