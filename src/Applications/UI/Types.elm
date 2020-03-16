module UI.Types exposing (..)

import Alfred exposing (Alfred)
import Browser
import Browser.Navigation as Nav
import Chunky exposing (..)
import Color exposing (Color)
import Common exposing (Switch)
import Conditional exposing (..)
import ContextMenu exposing (ContextMenu)
import Coordinates exposing (Viewport)
import Css exposing (url)
import Debouncer.Basic as Debouncer exposing (Debouncer)
import Dict exposing (Dict)
import Equalizer exposing (Knob)
import File exposing (File)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Http
import Json.Decode
import Keyboard
import LastFm
import Management
import Notifications exposing (Notification)
import Playlists exposing (Playlist, PlaylistTrack)
import Queue
import Sources
import Sources.Encoding as Sources
import Time
import Tracks
import Tracks.Encoding as Tracks
import UI.Authentication.Types as Authentication
import UI.DnD as DnD
import UI.Notifications
import UI.Page as Page exposing (Page)
import UI.Queue.Types as Queue
import UI.Reply as Reply exposing (Reply(..))
import UI.Sources as Sources
import UI.Sources.ContextMenu as Sources
import UI.Tracks as Tracks
import UI.Tracks.ContextMenu as Tracks
import Url exposing (Protocol(..), Url)
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
    { confirmation : Maybe String
    , currentTime : Time.Posix
    , darkMode : Bool
    , downloading : Maybe { notificationId : Int }
    , dnd : DnD.Model Int
    , focusedOnInput : Bool
    , isDragging : Bool
    , isLoading : Bool
    , isOnline : Bool
    , isTouchDevice : Bool
    , isUpgrading : Bool
    , lastFm : LastFm.Model
    , navKey : Nav.Key
    , page : Page
    , pressedKeys : List Keyboard.Key
    , processAutomatically : Bool
    , url : Url
    , viewport : Viewport

    -----------------------------------------
    -- Audio
    -----------------------------------------
    , audioDuration : Float
    , audioHasStalled : Bool
    , audioIsLoading : Bool
    , audioIsPlaying : Bool
    , audioPosition : Float
    , progress : Dict String Float
    , rememberProgress : Bool

    -----------------------------------------
    -- Backdrop
    -----------------------------------------
    , chosenBackdrop : Maybe String
    , extractedBackdropColor : Maybe Color
    , fadeInBackdrop : Bool
    , loadedBackdrops : List String

    -----------------------------------------
    -- Debouncing
    -----------------------------------------
    , debounce : Debouncer Msg Msg

    -----------------------------------------
    -- Equalizer
    -----------------------------------------
    , eqKnobOperation : Maybe Equalizer.KnobOperation
    , eqSettings : Equalizer.Settings

    -----------------------------------------
    -- Instances
    -----------------------------------------
    , alfred : Maybe (Alfred Msg)
    , contextMenu : Maybe (ContextMenu Reply)
    , notifications : UI.Notifications.Model

    -----------------------------------------
    -- Playlists
    -----------------------------------------
    , editPlaylistContext : Maybe { oldName : String, newName : String }
    , lastModifiedPlaylist : Maybe String
    , newPlaylistContext : Maybe String
    , playlists : List Playlist
    , playlistToActivate : Maybe String

    -----------------------------------------
    -- Queue
    -----------------------------------------
    , dontPlay : List Queue.Item
    , nowPlaying : Maybe Queue.Item
    , playedPreviously : List Queue.Item
    , playingNext : List Queue.Item
    , selectedQueueItem : Maybe Queue.Item

    --
    , repeat : Bool
    , shuffle : Bool

    -----------------------------------------
    -- 🦉 Nested
    -----------------------------------------
    , authentication : Authentication.State

    -----------------------------------------
    -- Children (TODO)
    -----------------------------------------
    , sources : Sources.Model
    , tracks : Tracks.Model
    }



-- 📣


type Msg
    = Bypass
    | Reply Reply
      -----------------------------------------
      -- Alfred
      -----------------------------------------
    | AssignAlfred (Alfred Msg)
    | GotAlfredInput String
    | SelectAlfredItem Int
      -----------------------------------------
      -- Audio
      -----------------------------------------
    | NoteProgress { trackId : String, progress : Float }
    | SetAudioDuration Float
    | SetAudioHasStalled Bool
    | SetAudioIsLoading Bool
    | SetAudioIsPlaying Bool
    | SetAudioPosition Float
    | Stop
    | TogglePlay
      -----------------------------------------
      -- Authentication (TODO: Move to Auth.Types)
      -----------------------------------------
    | AuthenticationBootFailure String
    | MissingSecretKey Json.Decode.Value
    | NotAuthenticated
    | RemoteStorageWebfinger RemoteStorage.Attributes (Result Http.Error String)
      -----------------------------------------
      -- Backdrop
      -----------------------------------------
    | ExtractedBackdropColor { r : Int, g : Int, b : Int }
    | ChooseBackdrop String
    | LoadBackdrop String
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
    | DnD (DnD.Msg Int)
    | FocusedOnInput
    | HideOverlay
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
    | ActivatePlaylist Playlist
    | AddTracksToPlaylist { playlistName : String, tracks : List PlaylistTrack }
    | CreatePlaylist
    | DeactivatePlaylist
    | DeletePlaylist { playlistName : String }
    | ModifyPlaylist
    | SetPlaylistCreationContext String
    | SetPlaylistModificationContext String String
    | ShowPlaylistListMenu Playlist Mouse.Event
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
      -- ⚗️ Adjunct
      -----------------------------------------
    | KeyboardMsg Keyboard.Msg
      -----------------------------------------
      -- 📭 Other
      -----------------------------------------
    | SetCurrentTime Time.Posix
    | SetIsOnline Bool
      -----------------------------------------
      -- 🦉 Nested
      -----------------------------------------
    | AuthenticationMsg Authentication.Msg
    | QueueMsg Queue.Msg
      -----------------------------------------
      -- Children (TODO)
      -----------------------------------------
    | SourcesMsg Sources.Msg
    | TracksMsg Tracks.Msg


type alias Organizer model =
    Management.Manager Msg model


type alias Manager =
    Organizer Model
