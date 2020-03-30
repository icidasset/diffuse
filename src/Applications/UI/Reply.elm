module UI.Reply exposing (Reply(..))

import Common exposing (Switch(..))
import Coordinates exposing (Coordinates)
import Playlists exposing (Playlist, PlaylistTrack)
import Sources exposing (Source)
import Tracks exposing (IdentifiedTrack, Track)
import UI.Page exposing (Page)
import User.Layer



-- 🌳


type Reply
    = Shunt
      --
    | CopyToClipboard String
    | GoToPage Page
    | ToggleLoadingScreen Switch
      -----------------------------------------
      -- Audio
      -----------------------------------------
    | Seek Float
    | TogglePlayPause
    | ToggleRememberProgress
      -----------------------------------------
      -- Authentication
      -----------------------------------------
    | ImportLegacyData
    | PingIpfsForAuth
    | PingTextileForAuth
    | ShowUpdateEncryptionKeyScreen User.Layer.Method
    | SignOut
      -----------------------------------------
      -- Context Menu
      -----------------------------------------
    | ContextMenuConfirmation String Reply
    | ReplyViaContextMenu Reply
    | ShowMoreAuthenticationOptions Coordinates
    | ShowPlaylistListMenu Coordinates Playlist
    | ShowTracksContextMenu Coordinates { alt : Bool } (List IdentifiedTrack)
    | ShowTracksViewMenu Coordinates (Maybe Tracks.Grouping)
      -----------------------------------------
      -- Last.fm
      -----------------------------------------
    | ConnectLastFm
    | DisconnectLastFm
      -----------------------------------------
      -- Notifications
      -----------------------------------------
    | DismissNotification { id : Int }
    | RemoveNotification { id : Int }
    | ShowErrorNotification String
    | ShowStickyErrorNotification String
    | ShowStickyErrorNotificationWithCode String String
    | ShowSuccessNotification String
    | ShowStickySuccessNotification String
    | ShowWarningNotification String
    | ShowStickyWarningNotification String
      -----------------------------------------
      -- Playlists
      -----------------------------------------
    | ActivatePlaylist Playlist
    | AddTracksToPlaylist { playlistName : String, tracks : List PlaylistTrack }
    | DeactivatePlaylist
    | GenerateDirectoryPlaylists
    | RemoveFromSelectedPlaylist Playlist (List IdentifiedTrack)
    | RemovePlaylistFromCollection { playlistName : String }
    | ReplacePlaylistInCollection Playlist
    | RequestAssistanceForPlaylists (List IdentifiedTrack)
      -----------------------------------------
      -- Queue
      -----------------------------------------
    | AddToQueue { inFront : Bool, tracks : List IdentifiedTrack }
    | MoveQueueItemToFirst { itemIndex : Int }
    | MoveQueueItemToLast { itemIndex : Int }
    | PlayTrack IdentifiedTrack
    | ResetQueue
    | RewindQueue
    | ShiftQueue
    | ToggleRepeat
    | ToggleShuffle
      -----------------------------------------
      -- Sources & Tracks
      -----------------------------------------
    | AddSourceToCollection Source
    | ClearTracksCache
    | DisableTracksGrouping
    | DownloadTracks String (List Track)
    | ExternalSourceAuthorization (String -> String)
    | ForceTracksRerender
    | GroupTracksBy Tracks.Grouping
    | ProcessSources (List Source)
    | RemoveSourceFromCollection { sourceId : String }
    | RemoveTracksFromCache (List Track)
    | RemoveTracksWithSourceId String
    | ScrollToNowPlaying
    | StoreTracksInCache (List Track)
    | ToggleCachedTracksOnly
    | ToggleDirectoryPlaylists { sourceId : String }
    | ToggleHideDuplicates
    | ToggleProcessAutomatically
      -----------------------------------------
      -- User Data
      -----------------------------------------
    | ChooseBackdrop String
    | Export
    | InsertDemo
    | LoadDefaultBackdrop
    | RequestImport
    | SaveEnclosedUserData
    | SaveFavourites
    | SavePlaylists
    | SaveProgress
    | SaveSettings
    | SaveSources
    | SaveTracks
