module UI.Types exposing (..)

import Alfred exposing (Alfred)
import Alien
import Browser
import Browser.Navigation as Nav
import Color exposing (Color)
import Common exposing (ServiceWorkerStatus, Switch)
import ContextMenu exposing (ContextMenu)
import Coordinates exposing (Viewport)
import Debouncer.Basic as Debouncer exposing (Debouncer)
import Dict exposing (Dict)
import Equalizer
import File exposing (File)
import Html.Events.Extra.Mouse as Mouse
import Http
import InfiniteList
import Json.Decode
import Keyboard
import LastFm
import Management
import Notifications exposing (Notification)
import Playlists exposing (Playlist, PlaylistTrack)
import Queue
import Random
import Sources exposing (Source)
import Time
import Tracks exposing (..)
import UI.Authentication.Types as Authentication
import UI.DnD as DnD
import UI.Page exposing (Page)
import UI.Queue.Types as Queue
import UI.Sources.Types as Sources
import UI.Tracks.Types as Tracks
import Url exposing (Url)
import Webnative



-- ⛩


type alias Flags =
    { buildTimestamp : Int
    , darkMode : Bool
    , initialTime : Int
    , isInstallingServiceWorker : Bool -- ie. Installing SW for the first time
    , isOnline : Bool
    , upgrade : Bool
    , version : String
    , viewport : Viewport
    }



-- 🌳


type alias Model =
    { buildTimestamp : Int
    , confirmation : Maybe String
    , currentTime : Time.Posix
    , currentTimeZone : Time.Zone
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
    , migratingData : Bool
    , navKey : Nav.Key
    , page : Page
    , pressedKeys : List Keyboard.Key
    , processAutomatically : Bool
    , serviceWorkerStatus : ServiceWorkerStatus
    , uuidSeed : Random.Seed
    , url : Url
    , version : String
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
    , eqSettings : Equalizer.Settings
    , showVolumeSlider : Bool

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
    , cachedCovers : Maybe (Dict String String)
    , cachedTracks : List String
    , cachedTracksOnly : Bool
    , cachingTracksInProgress : List String
    , covers : { arranged : List Tracks.Cover, harvested : List Tracks.Cover }
    , coverSelectionReducesPool : Bool
    , favourites : List Favourite
    , favouritesOnly : Bool
    , grouping : Maybe Grouping
    , hideDuplicates : Bool
    , scene : Scene
    , searchResults : Maybe (List String)
    , searchTerm : Maybe String
    , selectedCover : Maybe Cover
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
    | Pause
    | Play
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
    | AdjustVolume Float
    | ToggleVolumeSlider Switch
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
    | LostWindowFocus
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
    | AssistWithAddingTracksToPlaylist (List IdentifiedTrack)
    | AssistWithSelectingPlaylist
    | CreatePlaylist
    | DeactivatePlaylist
    | DeletePlaylist { playlistName : String }
    | DeselectPlaylist
    | ModifyPlaylist
    | MoveTrackInSelectedPlaylist { to : Int }
    | RemoveTracksFromPlaylist Playlist (List IdentifiedTrack)
    | SelectPlaylist Playlist
    | SetPlaylistCreationContext String
    | SetPlaylistModificationContext String String
    | ShowPlaylistListMenu Playlist Mouse.Event
      -----------------------------------------
      -- Routing
      -----------------------------------------
    | ChangeUrlUsingPage Page
    | LinkClicked Browser.UrlRequest
    | OpenUrlOnNewPage String
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
    | GotWebnativeResponse Webnative.Response
    | ImportFile File
    | ImportJson String
    | ImportLegacyData
    | InsertDemo
    | LoadEnclosedUserData Json.Decode.Value
    | LoadHypaethralUserData Json.Decode.Value
    | MigrateHypaethralUserData
    | RequestImport
    | SaveEnclosedUserData
    | SyncData
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
    | InstalledServiceWorker
    | InstallingServiceWorker
    | RedirectToBrain Alien.Event
    | ReloadApp
    | SetCurrentTime Time.Posix
    | SetCurrentTimeZone Time.Zone
    | SetIsOnline Bool


type alias Organizer model =
    Management.Manager Msg model


type alias Manager =
    Organizer Model
