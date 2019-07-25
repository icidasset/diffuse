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
    = GoToPage Page
    | StartedDragging
    | ToggleLoadingScreen Switch
      -----------------------------------------
      -- Authentication
      -----------------------------------------
    | ExternalAuth Authentication.Method String
    | ShowUpdateEncryptionKeyScreen Authentication.Method
    | SignOut
      -----------------------------------------
      -- Context Menu
      -----------------------------------------
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
    | ReplacePlaylistInCollection Playlist
      -----------------------------------------
      -- Queue
      -----------------------------------------
    | ActiveQueueItemChanged (Maybe Queue.Item)
    | FillQueue
    | PlayTrack IdentifiedTrack
    | ResetQueue
    | ShiftQueue
      -----------------------------------------
      -- Sources & Tracks
      -----------------------------------------
    | AddSourceToCollection Source
    | ExternalSourceAuthorization (String -> String)
    | ForceTracksRerender
    | PreloadNextTrack
    | ProcessSources
    | RemoveTracksFromCache (List Track)
    | RemoveTracksWithSourceId String
    | ReplaceSourceInCollection Source
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
