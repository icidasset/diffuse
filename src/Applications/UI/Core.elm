module UI.Core exposing (Msg(..))

import Authentication
import Authentication.RemoteStorage exposing (RemoteStorage)
import Browser
import Browser.Navigation as Nav
import Common exposing (Switch(..))
import ContextMenu exposing (ContextMenu)
import Coordinates exposing (Viewport)
import Debouncer.Basic as Debouncer exposing (Debouncer)
import File exposing (File)
import Http
import Json.Encode as Json
import Keyboard
import Notifications exposing (..)
import Playlists exposing (Playlist)
import Queue
import Time
import Tracks exposing (IdentifiedTrack, Track)
import UI.Alfred as Alfred
import UI.Authentication as Authentication
import UI.Backdrop as Backdrop
import UI.Equalizer as Equalizer
import UI.Notifications as Notifications
import UI.Page exposing (Page)
import UI.Playlists as Playlists
import UI.Queue as Queue
import UI.Reply exposing (Reply)
import UI.Sources as Sources
import UI.Tracks as Tracks
import Url exposing (Url)



-- 📣


type Msg
    = Bypass
    | CopyToClipboard String
    | Debounce (Debouncer.Msg Msg)
    | HideOverlay
    | KeyboardMsg Keyboard.Msg
    | LoadEnclosedUserData Json.Value
    | LoadHypaethralUserData Json.Value
    | MsgViaContextMenu Msg
    | Reply Reply
    | ResizedWindow ( Int, Int )
    | SetCurrentTime Time.Posix
    | SetIsOnline Bool
    | StoppedDragging
    | ToggleLoadingScreen Switch
      -----------------------------------------
      -- Audio
      -----------------------------------------
    | PlayPause
    | SetAudioDuration Float
    | SetAudioHasStalled Bool
    | SetAudioIsLoading Bool
    | SetAudioIsPlaying Bool
    | Stop
    | Unstall
      -----------------------------------------
      -- Authentication
      -----------------------------------------
    | AuthenticationBootFailure String
    | RemoteStorageWebfinger RemoteStorage (Result Http.Error String)
    | SyncUserData
      -----------------------------------------
      -- Children
      -----------------------------------------
    | AlfredMsg Alfred.Msg
    | AuthenticationMsg Authentication.Msg
    | BackdropMsg Backdrop.Msg
    | EqualizerMsg Equalizer.Msg
    | PlaylistsMsg Playlists.Msg
    | QueueMsg Queue.Msg
    | SourcesMsg Sources.Msg
    | TracksMsg Tracks.Msg
      -----------------------------------------
      -- Import / Export
      -----------------------------------------
    | Import File
    | ImportJson String
      -----------------------------------------
      -- Notifications
      -----------------------------------------
    | ShowNotification (Notification Reply)
      -----------------------------------------
      -- Page Transitions
      -----------------------------------------
    | PageChanged Page
      -----------------------------------------
      -- Playlists
      -----------------------------------------
    | RequestAssistanceForPlaylists (List IdentifiedTrack)
    | RemoveFromSelectedPlaylist Playlist (List IdentifiedTrack)
      -----------------------------------------
      -- Tracks Cache
      -----------------------------------------
    | FailedToStoreTracksInCache (List String)
    | FinishedStoringTracksInCache (List String)
    | RemoveFromTracksCache (List Track)
    | StoreInTracksCache (List Track)
      -----------------------------------------
      -- URL
      -----------------------------------------
    | ChangeUrlUsingPage Page
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
