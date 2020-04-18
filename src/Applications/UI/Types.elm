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
import InfiniteList
import Json.Decode
import Keyboard
import LastFm
import Management
import Notifications exposing (Notification)
import Playlists exposing (Playlist, PlaylistTrack)
import Queue
import Sources exposing (Source)
import Sources.Encoding as Sources
import Time
import Tracks exposing (..)
import Tracks.Encoding as Tracks
import UI.Authentication.Types as Authentication
import UI.DnD as DnD
import UI.Page as Page exposing (Page)
import UI.Queue.Types as Queue
import UI.Sources.Types as Sources
import UI.Tracks.Types as Tracks exposing (Scene)
import Url exposing (Protocol(..), Url)
import User.Layer exposing (..)



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
    , contextMenu : Maybe (ContextMenu Msg)
    , notifications : List (Notification Msg)

    -----------------------------------------
    -- Playlists
    -----------------------------------------
    , editPlaylistContext : Maybe { oldName : String, newName : String }
    , lastModifiedPlaylist : Maybe String
    , newPlaylistContext : Maybe String
    , playlists : List Playlist
    , playlistToActivate : Maybe String
    , selectedPlaylist : Maybe Playlist

    -----------------------------------------
    -- Queue
    -----------------------------------------
    , dontPlay : List Queue.Item
    , nowPlaying : Maybe Queue.Item
    , playedPreviously : List Queue.Item
    , playingNext : List Queue.Item
    , selectedQueueItem : Maybe Queue.Item

    -- Settings
    -----------
    , repeat : Bool
    , shuffle : Bool

    -----------------------------------------
    -- Sources
    -----------------------------------------
    , processingContext : List ( String, Float )
    , processingError : Maybe { error : String, sourceId : String }
    , processingNotificationId : Maybe Int
    , sourceForm : Sources.Form
    , sources : List Source

    -----------------------------------------
    -- Tracks
    -----------------------------------------
    , cachedTracks : List String
    , cachedTracksOnly : Bool
    , cachingTracksInProgress : List String
    , favourites : List Favourite
    , favouritesOnly : Bool
    , grouping : Maybe Grouping
    , hideDuplicates : Bool
    , scene : Scene
    , searchResults : Maybe (List String)
    , searchTerm : Maybe String
    , selectedTrackIndexes : List Int
    , sortBy : SortBy
    , sortDirection : SortDirection
    , tracks : Tracks.Collection

    -- List scene
    -------------
    , infiniteList : InfiniteList.Model

    -----------------------------------------
    -- 🦉 Nested
    -----------------------------------------
    , authentication : Authentication.State
    }



-- 📣


type Msg
    = Bypass
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
    | Seek Float
    | SetAudioDuration Float
    | SetAudioHasStalled Bool
    | SetAudioIsLoading Bool
    | SetAudioIsPlaying Bool
    | SetAudioPosition Float
    | Stop
    | TogglePlay
    | ToggleRememberProgress
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
    | ContextMenuConfirmation String Msg
    | CopyToClipboard String
    | Debounce (Debouncer.Msg Msg)
    | DismissNotification { id : Int }
    | DnD (DnD.Msg Int)
    | FocusedOnInput
    | HideOverlay
    | MsgViaContextMenu Msg
    | PreferredColorSchemaChanged { dark : Bool }
    | RemoveNotification { id : Int }
    | RemoveQueueSelection
    | RemoveTrackSelection
    | ResizedWindow ( Int, Int )
    | ShowNotification (Notification Msg)
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
    | DeselectPlaylist
    | ModifyPlaylist
    | MoveTrackInSelectedPlaylist { to : Int }
    | RemoveTracksFromPlaylist Playlist (List IdentifiedTrack)
    | RequestAssistanceForPlaylists (List IdentifiedTrack)
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
    | ConnectLastFm
    | DisconnectLastFm
    | GotLastFmSession (Result Http.Error String)
    | Scrobble { duration : Int, timestamp : Int, trackId : String }
      -----------------------------------------
      -- User
      -----------------------------------------
    | Export
    | ImportFile File
    | ImportJson String
    | ImportLegacyData
    | InsertDemo
    | LoadEnclosedUserData Json.Decode.Value
    | LoadHypaethralUserData Json.Decode.Value
    | RequestImport
    | SaveEnclosedUserData
      -----------------------------------------
      -- ⚗️ Adjunct
      -----------------------------------------
    | KeyboardMsg Keyboard.Msg
      -----------------------------------------
      -- 🦉 Nested
      -----------------------------------------
    | AuthenticationMsg Authentication.Msg
    | QueueMsg Queue.Msg
    | SourcesMsg Sources.Msg
    | TracksMsg Tracks.Msg
      -----------------------------------------
      -- 📭 Other
      -----------------------------------------
    | SetCurrentTime Time.Posix
    | SetIsOnline Bool


type alias Organizer model =
    Management.Manager Msg model


type alias Manager =
    Organizer Model
