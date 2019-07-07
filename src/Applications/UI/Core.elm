module UI.Core exposing (Flags, Model, Msg(..))

import Authentication
import Authentication.RemoteStorage exposing (RemoteStorage)
import Browser
import Browser.Navigation as Nav
import Common exposing (Switch(..))
import ContextMenu exposing (ContextMenu)
import Debouncer.Basic as Debouncer exposing (Debouncer)
import File exposing (File)
import Http
import Json.Encode as Json
import Keyboard
import Notifications exposing (..)
import Playlists exposing (Playlist)
import Queue
import Time
import Tracks exposing (IdentifiedTrack)
import UI.Alfred as Alfred
import UI.Authentication as Authentication
import UI.Backdrop as Backdrop
import UI.Equalizer as Equalizer
import UI.Page exposing (Page)
import UI.Playlists as Playlists
import UI.Queue.Core as Queue
import UI.Reply exposing (Reply)
import UI.Sources as Sources
import UI.Tracks.Core as Tracks
import Url exposing (Url)



-- ⛩


type alias Flags =
    { initialTime : Int
    , isOnline : Bool
    , viewport : Viewport
    }



-- 🌳


type alias Model =
    { contextMenu : Maybe (ContextMenu Msg)
    , currentTime : Time.Posix
    , debounce : Debouncer Msg Msg
    , isDragging : Bool
    , isLoading : Bool
    , isOnline : Bool
    , navKey : Nav.Key
    , notifications : List (Notification Msg)
    , page : Page
    , pressedKeys : List Keyboard.Key
    , url : Url
    , viewport : Viewport

    -----------------------------------------
    -- Audio
    -----------------------------------------
    , audioDuration : Float
    , audioHasStalled : Bool
    , audioIsLoading : Bool
    , audioIsPlaying : Bool

    -----------------------------------------
    -- Children
    -----------------------------------------
    , alfred : Alfred.Model
    , authentication : Authentication.Model
    , backdrop : Backdrop.Model
    , equalizer : Equalizer.Model
    , queue : Queue.Model
    , playlists : Playlists.Model
    , sources : Sources.Model
    , tracks : Tracks.Model
    }


type alias Viewport =
    { height : Float
    , width : Float
    }



-- 📣


type Msg
    = Bypass
    | Debounce (Debouncer.Msg Msg)
    | HideOverlay
    | KeyboardMsg Keyboard.Msg
    | LoadEnclosedUserData Json.Value
    | LoadHypaethralUserData Json.Value
    | MsgViaContextMenu Msg
    | Reply Reply
    | ResizedWindow ( Int, Int )
    | SetCurrentTime Time.Posix
    | SetIsOnline Bool
    | StoppedDragging
    | ToggleLoadingScreen Switch
      -----------------------------------------
      -- Audio
      -----------------------------------------
    | PlayPause
    | Seek Float
    | SetAudioDuration Float
    | SetAudioHasStalled Bool
    | SetAudioIsLoading Bool
    | SetAudioIsPlaying Bool
    | Stop
    | Unstall
      -----------------------------------------
      -- Authentication
      -----------------------------------------
    | AuthenticationBootFailure String
    | RemoteStorageWebfinger RemoteStorage (Result Http.Error String)
    | SyncUserData
      -----------------------------------------
      -- Brain
      -----------------------------------------
    | SignOut
      -----------------------------------------
      -- Children
      -----------------------------------------
    | AlfredMsg Alfred.Msg
    | AuthenticationMsg Authentication.Msg
    | BackdropMsg Backdrop.Msg
    | EqualizerMsg Equalizer.Msg
    | PlaylistsMsg Playlists.Msg
    | QueueMsg Queue.Msg
    | SourcesMsg Sources.Msg
    | TracksMsg Tracks.Msg
      -----------------------------------------
      -- Import / Export
      -----------------------------------------
    | Export
    | Import File
    | ImportJson String
    | RequestImport
      -----------------------------------------
      -- Notifications
      -----------------------------------------
    | DismissNotification { id : Int }
    | RemoveNotification { id : Int }
    | ShowNotification (Notification Msg)
      -----------------------------------------
      -- Page Transitions
      -----------------------------------------
    | PageChanged Page
      -----------------------------------------
      -- Playlists
      -----------------------------------------
    | RequestAssistanceForPlaylists (List IdentifiedTrack)
    | RemoveFromSelectedPlaylist Playlist (List IdentifiedTrack)
      -----------------------------------------
      -- URL
      -----------------------------------------
    | ChangeUrlUsingPage Page
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
