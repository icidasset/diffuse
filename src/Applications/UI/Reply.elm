module UI.Reply exposing (Reply(..))

import Common exposing (Switch(..))
import Coordinates exposing (Coordinates)
import Playlists exposing (Playlist, PlaylistTrack)
import Queue
import Sources exposing (Source)
import Tracks exposing (IdentifiedTrack, Track)
import UI.Page exposing (Page)
import User.Layer



-- 🌳


type Reply
    = Shunt
      --
    | GoToPage Page
    | StartedDragging
    | ToggleLoadingScreen Switch
      -----------------------------------------
      -- Audio
      -----------------------------------------
    | Seek Float
    | TogglePlayPause
      -----------------------------------------
      -- Authentication
      -----------------------------------------
    | ExternalAuth User.Layer.Method String
    | PingIpfsForAuth
    | PingTextileForAuth
    | ShowUpdateEncryptionKeyScreen User.Layer.Method
    | SignOut
      -----------------------------------------
      -- Context Menu
      -----------------------------------------
    | ReplyViaContextMenu Reply
    | ShowMoreAuthenticationOptions Coordinates
    | ShowPlaylistListMenu Coordinates Playlist
    | ShowQueueFutureMenu Coordinates { item : Queue.Item, itemIndex : Int }
    | ShowQueueHistoryMenu Coordinates { item : Queue.Item }
    | ShowSourceContextMenu Coordinates Source
    | ShowTracksContextMenu Coordinates (List IdentifiedTrack)
    | ShowTracksViewMenu Coordinates (Maybe Tracks.Grouping)
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
    | ActiveQueueItemChanged (Maybe Queue.Item)
    | AddToQueue { inFront : Bool, tracks : List IdentifiedTrack }
    | FillQueue
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
    | ExternalSourceAuthorization (String -> String)
    | ForceTracksRerender
    | GroupTracksBy Tracks.Grouping
    | PreloadNextTrack
    | ProcessSources (List Source)
    | RemoveSourceFromCollection { sourceId : String }
    | RemoveTracksFromCache (List Track)
    | RemoveTracksWithSourceId String
    | ReplaceSourceInCollection Source
    | ScrollToNowPlaying
    | StoreTracksInCache (List Track)
    | ToggleCachedTracksOnly
    | ToggleDirectoryPlaylists { sourceId : String }
    | ToggleHideDuplicates
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
    | SaveSettings
    | SaveSources
    | SaveTracks
    | SaveTracksFromBrain
