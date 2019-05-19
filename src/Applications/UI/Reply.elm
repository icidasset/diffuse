module UI.Reply exposing (Reply(..))

import Authentication
import Common exposing (Switch(..))
import Coordinates exposing (Coordinates)
import Playlists exposing (Playlist)
import Queue
import Sources exposing (Source)
import Tracks exposing (IdentifiedTrack)
import UI.Page exposing (Page)



-- 🌳


type Reply
    = ExternalAuth Authentication.Method String
    | GoToPage Page
    | StartedDragging
    | ToggleLoadingScreen Switch
      -----------------------------------------
      -- Context Menu
      -----------------------------------------
    | ShowMoreAuthenticationOptions Coordinates
    | ShowSourceContextMenu Coordinates Source
    | ShowTracksContextMenu Coordinates (List IdentifiedTrack)
    | ShowTracksViewMenu Coordinates (Maybe Tracks.Grouping)
      -----------------------------------------
      -- Notifications
      -----------------------------------------
    | DismissNotification { id : Int }
    | ShowErrorNotification String
    | ShowErrorNotificationWithCode String String
    | ShowSuccessNotification String
    | ShowWarningNotification String
      -----------------------------------------
      -- Playlists
      -----------------------------------------
    | ActivatePlaylist Playlist
    | DeactivatePlaylist
    | GenerateDirectoryPlaylists
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
    | ProcessSources
    | RemoveTracksWithSourceId String
    | ReplaceSourceInCollection Source
      -----------------------------------------
      -- User Data
      -----------------------------------------
    | InsertDemo
    | SaveEnclosedUserData
    | SaveFavourites
    | SaveSettings
    | SaveSources
    | SaveTracks
