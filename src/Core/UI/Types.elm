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
import Playlists exposing (Playlist, PlaylistTrackWithoutMetadata)
import Queue
import Random
import Sources exposing (Source)
import Theme
import Time
import Tracks exposing (..)
import UI.Audio.Types exposing (DurationChangeEvent, ErrorAudioEvent, GenericAudioEvent, NowPlaying, PlaybackStateEvent, TimeUpdatedEvent)
import UI.DnD as DnD
import UI.Page exposing (Page)
import UI.Queue.Types as Queue
import UI.Sources.Types as Sources
import UI.Syncing.Types as Syncing
import UI.Tracks.Types as Tracks
import Url exposing (Url)



-- ⛩


type alias Flags =
    { buildTimestamp : Int
    , darkMode : Bool
    , initialTime : Int
    , isInstallingServiceWorker : Bool -- ie. Installing SW for the first time
    , isOnline : Bool
    , isTauri : Bool
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
    , isTauri : Bool
    , isTouchDevice : Bool
    , lastFm : LastFm.Model
    , navKey : Nav.Key
    , page : Page
    , pressedKeys : List Keyboard.Key
    , processAutomatically : Bool
    , serviceWorkerStatus : ServiceWorkerStatus
    , theme : Maybe Theme.Id
    , uuidSeed : Random.Seed
    , url : Url
    , version : String
    , viewport : Viewport

    -----------------------------------------
    -- Audio
    -----------------------------------------
    , audioElements : List Queue.EngineItem
    , nowPlaying : Maybe NowPlaying
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
    , preloadDebouncer : Debouncer Msg Msg
    , progressDebouncer : Debouncer Msg Msg
    , resizeDebouncer : Debouncer Msg Msg
    , searchDebouncer : Debouncer Msg Msg

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
    , lastModifiedPlaylist : Maybe { collection : Bool, name : String }
    , newPlaylistContext : Maybe String
    , playlists : List Playlist
    , playlistToActivate : Maybe String
    , selectedPlaylist : Maybe Playlist

    -----------------------------------------
    -- Queue
    -----------------------------------------
    , dontPlay : List Queue.Item
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
    , syncing : Syncing.State
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
    | AudioDurationChange DurationChangeEvent
    | AudioError ErrorAudioEvent
    | AudioEnded GenericAudioEvent
    | AudioHasLoaded GenericAudioEvent
    | AudioIsLoading GenericAudioEvent
    | AudioPlaybackStateChanged PlaybackStateEvent
    | AudioPreloadDebounce (Debouncer.Msg Msg)
    | AudioTimeUpdated TimeUpdatedEvent
    | NoteProgress { trackId : String, progress : Float }
    | NoteProgressDebounce (Debouncer.Msg Msg)
    | Pause
    | Play
    | Seek { trackId : String, progress : Float }
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
    | AssistWithChangingTheme
    | Blur
    | ChangeTheme Theme.Id
    | ContextMenuConfirmation String Msg
    | CopyToClipboard String
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
    | ResizeDebounce (Debouncer.Msg Msg)
    | ResizedWindow ( Int, Int )
    | SearchDebounce (Debouncer.Msg Msg)
    | ShowNotification (Notification Msg)
    | SetIsTouchDevice Bool
    | StoppedDragging
    | ToggleLoadingScreen Switch
      -----------------------------------------
      -- Playlists
      -----------------------------------------
    | ActivatePlaylist Playlist
    | AddTracksToPlaylist { collection : Bool, playlistName : String, tracks : List PlaylistTrackWithoutMetadata }
    | AssistWithAddingTracksToCollection (List IdentifiedTrack)
    | AssistWithAddingTracksToPlaylist (List IdentifiedTrack)
    | AssistWithSelectingPlaylist
    | ConvertCollectionToPlaylist { name : String }
    | ConvertPlaylistToCollection { name : String }
    | CreateCollection
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
    | TogglePlaylistVisibility Playlist
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
    | ImportFile File
    | ImportJson String
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
    | SyncingMsg Syncing.Msg
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
