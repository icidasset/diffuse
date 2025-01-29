module UI exposing (main)

import Alien
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Common exposing (ServiceWorkerStatus(..), Switch(..))
import Debouncer.Basic as Debouncer
import Dict
import Equalizer
import InfiniteList
import Json.Decode as Json
import Keyboard
import LastFm
import Maybe.Extra as Maybe
import Notifications
import Random
import Return
import Task
import Time
import Tracks
import UI.Adjunct as Adjunct
import UI.Alfred.State as Alfred
import UI.Audio.State as Audio
import UI.Backdrop as Backdrop
import UI.Common.State as Common
import UI.DnD as DnD
import UI.Equalizer.State as Equalizer
import UI.Interface.State as Interface
import UI.Other.State as Other
import UI.Page as Page
import UI.Playlists.State as Playlists
import UI.Ports as Ports
import UI.Queue.State as Queue
import UI.Queue.Types as Queue
import UI.Routing.State as Routing
import UI.Services.State as Services
import UI.Sources.Form
import UI.Sources.State as Sources
import UI.Sources.Types as Sources
import UI.Syncing.State as Syncing
import UI.Syncing.Types as Syncing
import UI.Tracks.State as Tracks
import UI.Tracks.Types as Tracks
import UI.Types exposing (..)
import UI.User.State.Export as User
import UI.User.State.Import as User
import UI.View exposing (view)
import Url exposing (Url)
import Url.Ext as Url



-- ⛩


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- 🌳


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        rewrittenUrl =
            Page.rewriteUrl url

        maybePage =
            Page.fromUrl rewrittenUrl

        page =
            Maybe.withDefault Page.Index maybePage

        serviceWorkerStatus =
            if flags.isInstallingServiceWorker then
                InstallingInitial

            else
                Activated
    in
    { buildTimestamp = flags.buildTimestamp
    , confirmation = Nothing
    , currentTime = Time.millisToPosix flags.initialTime
    , currentTimeZone = Time.utc
    , darkMode = flags.darkMode
    , downloading = Nothing
    , dnd = DnD.initialModel
    , focusedOnInput = False
    , isDragging = False
    , isLoading = True
    , isOnline = flags.isOnline
    , isTauri = flags.isTauri
    , isTouchDevice = False
    , lastFm = LastFm.initialModel
    , navKey = key
    , page = page
    , pressedKeys = []
    , processAutomatically = True
    , serviceWorkerStatus = serviceWorkerStatus
    , theme = Nothing
    , uuidSeed = Random.initialSeed flags.initialTime
    , url = url
    , version = flags.version
    , viewport = flags.viewport

    -----------------------------------------
    -- Audio
    -----------------------------------------
    , audioElements = []
    , nowPlaying = Nothing
    , progress = Dict.empty
    , rememberProgress = True

    -----------------------------------------
    -- Backdrop
    -----------------------------------------
    , chosenBackdrop = Nothing
    , extractedBackdropColor = Nothing
    , fadeInBackdrop = True
    , loadedBackdrops = []

    -----------------------------------------
    -- Debouncing
    -----------------------------------------
    , preloadDebouncer =
        30
            |> Debouncer.fromSeconds
            |> Debouncer.debounce
            |> Debouncer.toDebouncer
    , progressDebouncer =
        30
            |> Debouncer.fromSeconds
            |> Debouncer.throttle
            |> Debouncer.emitWhenUnsettled Nothing
            |> Debouncer.toDebouncer
    , resizeDebouncer =
        0.25
            |> Debouncer.fromSeconds
            |> Debouncer.debounce
            |> Debouncer.toDebouncer
    , searchDebouncer =
        0.5
            |> Debouncer.fromSeconds
            |> Debouncer.debounce
            |> Debouncer.toDebouncer

    -----------------------------------------
    -- Equalizer
    -----------------------------------------
    , eqSettings = Equalizer.defaultSettings
    , showVolumeSlider = False

    -----------------------------------------
    -- Instances
    -----------------------------------------
    , alfred = Nothing
    , contextMenu = Nothing
    , notifications = []

    -----------------------------------------
    -- Playlists
    -----------------------------------------
    , editPlaylistContext = Nothing
    , lastModifiedPlaylist = Nothing
    , newPlaylistContext = Nothing
    , playlists = []
    , playlistToActivate = Nothing
    , selectedPlaylist = Nothing

    -----------------------------------------
    -- Queue
    -----------------------------------------
    , dontPlay = []
    , playedPreviously = []
    , playingNext = []
    , selectedQueueItem = Nothing

    --
    , repeat = False
    , shuffle = False

    -----------------------------------------
    -- Sources
    -----------------------------------------
    , processingContext = []
    , processingError = Nothing
    , processingNotificationId = Nothing
    , sources = []
    , sourceForm = UI.Sources.Form.initialModel

    -----------------------------------------
    -- Tracks
    -----------------------------------------
    , cachedCovers = Nothing
    , cachedTracks = []
    , cachedTracksOnly = False
    , cachingTracksInProgress = []
    , covers = { arranged = [], harvested = [] }
    , coverSelectionReducesPool = True
    , favourites = []
    , favouritesOnly = False
    , grouping = Nothing
    , hideDuplicates = False
    , scene = Tracks.Covers
    , searchResults = Nothing
    , searchTerm = Nothing
    , selectedCover = Nothing
    , selectedTrackIndexes = []
    , sortBy = Tracks.Album
    , sortDirection = Tracks.Asc
    , tracks = Tracks.emptyCollection

    -- List scene
    -------------
    , infiniteList = InfiniteList.init

    -----------------------------------------
    -- 🦉 Nested
    -----------------------------------------
    , syncing = Syncing.initialModel url
    }
        |> Routing.transition
            page
        |> Return.command
            (url
                |> Syncing.initialCommand
                |> Cmd.map SyncingMsg
            )
        |> Return.command
            (if Maybe.isNothing maybePage then
                Routing.resetUrl key url page

             else
                case Url.action url of
                    [ "authenticate", "dropbox" ] ->
                        Routing.resetUrl key url page

                    _ ->
                        Cmd.none
            )
        |> Return.command
            (Task.perform SetCurrentTime Time.now)
        |> Return.command
            (Task.perform SetCurrentTimeZone Time.here)



-- 📣


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        Bypass ->
            Return.singleton

        -----------------------------------------
        -- Alfred
        -----------------------------------------
        AssignAlfred a ->
            Alfred.assign a

        GotAlfredInput a ->
            Alfred.gotInput a

        SelectAlfredItem a ->
            Alfred.runAction a

        -----------------------------------------
        -- Audio
        -----------------------------------------
        AudioDurationChange a ->
            Audio.durationChange a

        AudioEnded a ->
            Audio.ended a

        AudioError a ->
            Audio.error a

        AudioHasLoaded a ->
            Audio.hasLoaded a

        AudioIsLoading a ->
            Audio.isLoading a

        AudioPlaybackStateChanged a ->
            Audio.playbackStateChanged a

        AudioPreloadDebounce a ->
            Audio.preloadDebounce update a

        AudioTimeUpdated a ->
            Audio.timeUpdated a

        NoteProgress a ->
            Audio.noteProgress a

        NoteProgressDebounce a ->
            Audio.noteProgressDebounce update a

        Pause ->
            Audio.pause

        Play ->
            Audio.play

        Seek a ->
            Audio.seek a

        Stop ->
            Audio.stop

        TogglePlay ->
            Audio.playPause

        ToggleRememberProgress ->
            Audio.toggleRememberProgress

        -----------------------------------------
        -- Backdrop
        -----------------------------------------
        ExtractedBackdropColor a ->
            Backdrop.extractedBackdropColor a

        ChooseBackdrop a ->
            Backdrop.chooseBackdrop a

        LoadBackdrop a ->
            Backdrop.loadBackdrop a

        -----------------------------------------
        -- Equalizer
        -----------------------------------------
        AdjustVolume a ->
            Equalizer.adjustVolume a

        ToggleVolumeSlider a ->
            Equalizer.toggleVolumeSlider a

        -----------------------------------------
        -- Interface
        -----------------------------------------
        AssistWithChangingTheme ->
            Interface.assistWithChangingTheme

        Blur ->
            Interface.blur

        ChangeTheme a ->
            Interface.changeTheme a

        ContextMenuConfirmation a b ->
            Interface.contextMenuConfirmation a b

        CopyToClipboard a ->
            Interface.copyToClipboard a

        DismissNotification a ->
            Common.dismissNotification a

        DnD a ->
            Interface.dnd a

        FocusedOnInput ->
            Interface.focusedOnInput

        HideOverlay ->
            Interface.hideOverlay

        LostWindowFocus ->
            Interface.lostWindowFocus

        MsgViaContextMenu a ->
            Interface.msgViaContextMenu a

        PreferredColorSchemaChanged a ->
            Interface.preferredColorSchemaChanged a

        RemoveNotification a ->
            Interface.removeNotification a

        RemoveQueueSelection ->
            Interface.removeQueueSelection

        RemoveTrackSelection ->
            Interface.removeTrackSelection

        ResizeDebounce a ->
            Interface.resizeDebounce update a

        ResizedWindow a ->
            Interface.resizedWindow a

        SearchDebounce a ->
            Interface.searchDebounce update a

        SetIsTouchDevice a ->
            Interface.setIsTouchDevice a

        ShowNotification a ->
            Common.showNotification a

        StoppedDragging ->
            Interface.stoppedDragging

        ToggleLoadingScreen a ->
            Common.toggleLoadingScreen a

        -----------------------------------------
        -- Playlists
        -----------------------------------------
        ActivatePlaylist a ->
            Playlists.activate a

        AddTracksToPlaylist a ->
            Playlists.addTracksToPlaylist a

        AssistWithAddingTracksToCollection a ->
            Playlists.assistWithAddingTracksToCollection a

        AssistWithAddingTracksToPlaylist a ->
            Playlists.assistWithAddingTracksToPlaylist a

        AssistWithSelectingPlaylist ->
            Playlists.assistWithSelectingPlaylist

        ConvertCollectionToPlaylist a ->
            Playlists.convertCollectionToPlaylist a

        ConvertPlaylistToCollection a ->
            Playlists.convertPlaylistToCollection a

        CreateCollection ->
            Playlists.createCollection

        CreatePlaylist ->
            Playlists.createPlaylist

        DeactivatePlaylist ->
            Playlists.deactivate

        DeletePlaylist a ->
            Playlists.delete a

        DeselectPlaylist ->
            Playlists.deselect

        ModifyPlaylist ->
            Playlists.modify

        MoveTrackInSelectedPlaylist a ->
            Playlists.moveTrackInSelected a

        RemoveTracksFromPlaylist a b ->
            Playlists.removeTracks a b

        SelectPlaylist a ->
            Playlists.select a

        SetPlaylistCreationContext a ->
            Playlists.setCreationContext a

        SetPlaylistModificationContext a b ->
            Playlists.setModificationContext a b

        ShowPlaylistListMenu a b ->
            Playlists.showListMenu a b

        TogglePlaylistVisibility a ->
            Playlists.toggleVisibility a

        -----------------------------------------
        -- Routing
        -----------------------------------------
        ChangeUrlUsingPage a ->
            Common.changeUrlUsingPage a

        LinkClicked a ->
            Routing.linkClicked a

        OpenUrlOnNewPage a ->
            Routing.openUrlOnNewPage a

        PageChanged a ->
            Routing.transition a

        UrlChanged a ->
            Routing.urlChanged a

        -----------------------------------------
        -- Services
        -----------------------------------------
        ConnectLastFm ->
            Services.connectLastFm

        DisconnectLastFm ->
            Services.disconnectLastFm

        GotLastFmSession a ->
            Services.gotLastFmSession a

        Scrobble a ->
            Services.scrobble a

        -----------------------------------------
        -- User
        -----------------------------------------
        Export ->
            User.export

        ImportFile a ->
            User.importFile a

        ImportJson a ->
            User.importJson a

        InsertDemo ->
            User.insertDemo

        LoadEnclosedUserData a ->
            User.loadEnclosedUserData a

        LoadHypaethralUserData a ->
            User.loadHypaethralUserData a

        RequestImport ->
            User.requestImport

        SaveEnclosedUserData ->
            User.saveEnclosedUserData

        -----------------------------------------
        -- ⚗️ Adjunct
        -----------------------------------------
        KeyboardMsg a ->
            Adjunct.keyboardInput a

        -----------------------------------------
        -- 🦉 Nested
        -----------------------------------------
        SyncingMsg a ->
            Syncing.update a

        QueueMsg a ->
            Queue.update a

        SourcesMsg a ->
            Sources.update a

        TracksMsg a ->
            Tracks.update a

        -----------------------------------------
        -- 📭 Other
        -----------------------------------------
        InstalledServiceWorker ->
            Other.installedServiceWorker

        InstallingServiceWorker ->
            Other.installingServiceWorker

        RedirectToBrain a ->
            Other.redirectToBrain a

        ReloadApp ->
            Other.reloadApp

        SetCurrentTime a ->
            Other.setCurrentTime a

        SetCurrentTimeZone a ->
            Other.setCurrentTimeZone a

        SetIsOnline a ->
            Other.setIsOnline a



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.fromAlien alien

        -----------------------------------------
        -- Audio
        -----------------------------------------
        , Ports.audioDurationChange AudioDurationChange
        , Ports.audioEnded AudioEnded
        , Ports.audioError AudioError
        , Ports.audioPlaybackStateChanged AudioPlaybackStateChanged
        , Ports.audioIsLoading AudioIsLoading
        , Ports.audioHasLoaded AudioHasLoaded
        , Ports.audioTimeUpdated AudioTimeUpdated
        , Ports.requestPause (always Pause)
        , Ports.requestPlay (always Play)
        , Ports.requestPlayPause (always TogglePlay)
        , Ports.requestStop (always Stop)

        -----------------------------------------
        -- Backdrop
        -----------------------------------------
        , Ports.setAverageBackgroundColor ExtractedBackdropColor

        -----------------------------------------
        -- Interface
        -----------------------------------------
        , Browser.Events.onResize Interface.onResize
        , Ports.indicateTouchDevice (\_ -> SetIsTouchDevice True)
        , Ports.lostWindowFocus (always LostWindowFocus)
        , Ports.preferredColorSchemaChanged PreferredColorSchemaChanged
        , Ports.showErrorNotification (Notifications.error >> ShowNotification)
        , Ports.showStickyErrorNotification (Notifications.stickyError >> ShowNotification)

        -----------------------------------------
        -- Queue
        -----------------------------------------
        , Ports.requestNext (\_ -> QueueMsg Queue.Shift)
        , Ports.requestPrevious (\_ -> QueueMsg Queue.Rewind)

        -----------------------------------------
        -- Services
        -----------------------------------------
        , Ports.scrobble Scrobble

        -----------------------------------------
        -- Tracks
        -----------------------------------------
        , Ports.downloadTracksFinished (\_ -> TracksMsg Tracks.DownloadFinished)
        , Ports.insertCoverCache (TracksMsg << Tracks.InsertCoverCache)

        -----------------------------------------
        -- 📭 Other
        -----------------------------------------
        , Ports.installedNewServiceWorker (\_ -> InstalledServiceWorker)
        , Ports.installingNewServiceWorker (\_ -> InstallingServiceWorker)
        , Ports.refreshedAccessToken (Alien.broadcast Alien.RefreshedAccessToken >> RedirectToBrain)
        , Ports.setIsOnline SetIsOnline
        , Sub.map KeyboardMsg Keyboard.subscriptions
        , Time.every (60 * 1000) SetCurrentTime
        ]



-- 👽


alien : Alien.Event -> Msg
alien event =
    case ( event.error, Alien.tagFromString event.tag ) of
        ( Nothing, Just tag ) ->
            translateAlienData tag event.data

        ( Just err, Just tag ) ->
            translateAlienError tag event.data err

        _ ->
            Bypass


translateAlienData : Alien.Tag -> Json.Value -> Msg
translateAlienData tag data =
    case tag of
        Alien.AddTracks ->
            TracksMsg (Tracks.Add data)

        Alien.FinishedProcessingSource ->
            SourcesMsg (Sources.FinishedProcessingSource data)

        Alien.FinishedProcessingSources ->
            SourcesMsg Sources.FinishedProcessing

        Alien.GotCachedCover ->
            TracksMsg (Tracks.GotCachedCover data)

        Alien.HideLoadingScreen ->
            ToggleLoadingScreen Off

        Alien.LoadEnclosedUserData ->
            LoadEnclosedUserData data

        Alien.LoadHypaethralUserData ->
            LoadHypaethralUserData data

        Alien.ReloadTracks ->
            TracksMsg (Tracks.Reload data)

        Alien.RemoveTracksByPath ->
            TracksMsg (Tracks.RemoveByPaths data)

        Alien.ReportProcessingError ->
            SourcesMsg (Sources.ReportProcessingError data)

        Alien.ReportProcessingProgress ->
            SourcesMsg (Sources.ReportProcessingProgress data)

        Alien.SearchTracks ->
            TracksMsg (Tracks.SetSearchResults data)

        Alien.StartedSyncing ->
            SyncingMsg (Syncing.StartedSyncing data)

        Alien.StoreTracksInCache ->
            TracksMsg (Tracks.StoredInCache data Nothing)

        Alien.SyncMethod ->
            SyncingMsg (Syncing.GotSyncMethod data)

        Alien.UpdateSourceData ->
            SourcesMsg (Sources.UpdateSourceData data)

        _ ->
            Bypass


translateAlienError : Alien.Tag -> Json.Value -> String -> Msg
translateAlienError tag data err =
    case tag of
        Alien.StoreTracksInCache ->
            TracksMsg (Tracks.StoredInCache data <| Just err)

        _ ->
            if String.startsWith "There seems to be existing data that's encrypted, I will need the passphrase" err then
                SyncingMsg (Syncing.NeedEncryptionKey { error = err })

            else
                ShowNotification (Notifications.stickyError err)
