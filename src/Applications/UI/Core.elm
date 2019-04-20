module UI.Core exposing (Flags, Model, Msg(..))

import Authentication
import Authentication.RemoteStorage exposing (RemoteStorage)
import Browser
import Browser.Navigation as Nav
import Common exposing (Switch(..))
import ContextMenu exposing (ContextMenu)
import File exposing (File)
import Http
import Json.Encode as Json
import Notifications exposing (..)
import Queue
import Time
import UI.Authentication as Authentication
import UI.Backdrop as Backdrop
import UI.Equalizer as Equalizer
import UI.Page exposing (Page)
import UI.Queue.Core as Queue
import UI.Sources as Sources
import UI.Tracks.Core as Tracks
import Url exposing (Url)



-- ⛩


type alias Flags =
    { initialTime : Int
    , viewport : Viewport
    }



-- 🌳


type alias Model =
    { contextMenu : Maybe (ContextMenu Msg)
    , currentTime : Time.Posix
    , isLoading : Bool
    , navKey : Nav.Key
    , notifications : List (Notification Msg)
    , page : Page
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
    , authentication : Authentication.Model
    , backdrop : Backdrop.Model
    , equalizer : Equalizer.Model
    , queue : Queue.Model
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
    | LoadEnclosedUserData Json.Value
    | LoadHypaethralUserData Json.Value
    | SetCurrentTime Time.Posix
    | ToggleLoadingScreen Switch
      -----------------------------------------
      -- Audio
      -----------------------------------------
    | Pause
    | Play
    | Seek Float
    | SetAudioDuration Float
    | SetAudioHasStalled Bool
    | SetAudioIsLoading Bool
    | SetAudioIsPlaying Bool
    | Unstall
      -----------------------------------------
      -- Authentication
      -----------------------------------------
    | RemoteStorageWebfinger RemoteStorage (Result Http.Error String)
      -----------------------------------------
      -- Brain
      -----------------------------------------
    | SignOut
      -----------------------------------------
      -- Children
      -----------------------------------------
    | AuthenticationMsg Authentication.Msg
    | BackdropMsg Backdrop.Msg
    | EqualizerMsg Equalizer.Msg
    | QueueMsg Queue.Msg
    | SourcesMsg Sources.Msg
    | TracksMsg Tracks.Msg
      -----------------------------------------
      -- Context Menu
      -----------------------------------------
    | HideContextMenu
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
      -- URL
      -----------------------------------------
    | ChangeUrlUsingPage Page
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
