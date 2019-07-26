module UI.Reply exposing (Reply(..))

import Authentication
import Common exposing (Switch(..))
import Coordinates exposing (Coordinates)
import Playlists exposing (Playlist, PlaylistTrack)
import Queue
import Sources exposing (Source)
import Tracks exposing (IdentifiedTrack, Track)
import UI.Page exposing (Page)



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
    | ExternalAuth Authentication.Method String
    | PingIpfsForAuth
    | ShowUpdateEncryptionKeyScreen Authentication.Method
    | SignOut
      -----------------------------------------
      -- Context Menu
      -----------------------------------------
    | ReplyViaContextMenu Reply
    | ShowMoreAuthenticationOptions Coordinates
    | ShowPlaylistListMenu Coordinates Playlist
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
    | ProcessSources
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
