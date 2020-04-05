module UI exposing (main)

import Alien
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Chunky exposing (..)
import Common exposing (Switch(..))
import Conditional exposing (..)
import Css exposing (url)
import Debouncer.Basic as Debouncer
import Dict
import Equalizer
import InfiniteList
import Keyboard
import LastFm
import Maybe.Extra as Maybe
import Notifications
import Playlists.Encoding as Playlists
import Queue
import Return
import Sources
import Sources.Encoding as Sources
import Task
import Time
import Tracks
import Tracks.Encoding as Tracks
import UI.Adjunct as Adjunct
import UI.Alfred.State as Alfred
import UI.Alien as Alien
import UI.Audio.State as Audio
import UI.Authentication.State as Authentication
import UI.Backdrop as Backdrop
import UI.Common.State as Common
import UI.DnD as DnD
import UI.Equalizer.State as Equalizer
import UI.Equalizer.View as Equalizer
import UI.Interface.State as Interface
import UI.Other.State as Other
import UI.Page as Page
import UI.Playlists.ContextMenu as Playlists
import UI.Playlists.State as Playlists
import UI.Ports as Ports
import UI.Queue.State as Queue
import UI.Queue.Types as Queue
import UI.Reply.Translate as Reply
import UI.Routing.State as Routing
import UI.Services.State as Services
import UI.Sources.ContextMenu as Sources
import UI.Sources.Form
import UI.Sources.State as Sources
import UI.Tracks.ContextMenu as Tracks
import UI.Tracks.State as Tracks
import UI.Tracks.Types as Tracks
import UI.Types as UI exposing (..)
import UI.User.State.Export as User
import UI.User.State.Import as User
import UI.View exposing (view)
import Url exposing (Protocol(..), Url)
import User.Layer exposing (..)



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
    in
    { confirmation = Nothing
    , currentTime = Time.millisToPosix flags.initialTime
    , darkMode = flags.darkMode
    , downloading = Nothing
    , dnd = DnD.initialModel
    , focusedOnInput = False
    , isDragging = False
    , isLoading = True
    , isOnline = flags.isOnline
    , isTouchDevice = False
    , isUpgrading = flags.upgrade
    , lastFm = LastFm.initialModel
    , navKey = key
    , page = page
    , pressedKeys = []
    , processAutomatically = True
    , url = url
    , viewport = flags.viewport

    -----------------------------------------
    -- Audio
    -----------------------------------------
    , audioDuration = 0
    , audioHasStalled = False
    , audioIsLoading = False
    , audioIsPlaying = False
    , audioPosition = 0
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
    , debounce =
        0.25
            |> Debouncer.fromSeconds
            |> Debouncer.debounce
            |> Debouncer.toDebouncer

    -----------------------------------------
    -- Equalizer
    -----------------------------------------
    , eqKnobOperation = Nothing
    , eqSettings = Equalizer.defaultSettings

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
    , nowPlaying = Nothing
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
    , cachedTracks = []
    , cachedTracksOnly = False
    , cachingTracksInProgress = []
    , favourites = []
    , favouritesOnly = False
    , grouping = Nothing
    , hideDuplicates = False
    , scene = Tracks.List
    , searchResults = Nothing
    , searchTerm = Nothing
    , selectedTrackIndexes = []
    , sortBy = Tracks.Artist
    , sortDirection = Tracks.Asc
    , tracks = Tracks.emptyCollection

    -- List scene
    -------------
    , infiniteList = InfiniteList.init

    -----------------------------------------
    -- 🦉 Nested
    -----------------------------------------
    , authentication = Authentication.initialModel url
    }
        |> Routing.transition
            page
        |> Return.command
            (Maybe.unwrap
                Cmd.none
                (Routing.resetUrl key url)
                maybePage
            )
        |> Return.command
            (Task.perform SetCurrentTime Time.now)



-- 📣


update : Msg -> Model -> ( Model, Cmd Msg )
update msg =
    case msg of
        Bypass ->
            Return.singleton

        Reply reply ->
            Reply.translate reply

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
        NoteProgress a ->
            Audio.noteProgress a

        SetAudioDuration a ->
            Audio.setDuration a

        SetAudioHasStalled a ->
            Audio.setHasStalled a

        SetAudioIsLoading a ->
            Audio.setIsLoading a

        SetAudioIsPlaying a ->
            Audio.setIsPlaying a

        SetAudioPosition a ->
            Audio.setPosition a

        Stop ->
            Audio.stop

        TogglePlay ->
            Audio.playPause

        -----------------------------------------
        -- Authentication (TODO: Move)
        -----------------------------------------
        AuthenticationBootFailure a ->
            Authentication.bootFailure a

        MissingSecretKey a ->
            Authentication.missingSecretKey a

        NotAuthenticated ->
            Authentication.notAuthenticated

        RemoteStorageWebfinger a b ->
            Authentication.remoteStorageWebfinger a b

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
        ActivateKnob a b ->
            Equalizer.activateKnob a b

        AdjustKnob a ->
            Equalizer.adjustKnob a

        DeactivateKnob _ ->
            Equalizer.deactivateKnob

        ResetKnob a ->
            Equalizer.resetKnob a

        -----------------------------------------
        -- Interface
        -----------------------------------------
        Blur ->
            Interface.blur

        Debounce a ->
            Interface.debounce update a

        DnD a ->
            Interface.dnd a

        FocusedOnInput ->
            Interface.focusedOnInput

        HideOverlay ->
            Interface.hideOverlay

        PreferredColorSchemaChanged a ->
            Interface.preferredColorSchemaChanged a

        RemoveQueueSelection ->
            Interface.removeQueueSelection

        RemoveTrackSelection ->
            Interface.removeTrackSelection

        ResizedWindow a ->
            Interface.resizedWindow a

        SetIsTouchDevice a ->
            Interface.setIsTouchDevice a

        ShowNotification a ->
            Common.showNotification a

        StoppedDragging ->
            Interface.stoppedDragging

        UI.ToggleLoadingScreen a ->
            Common.toggleLoadingScreen a

        -----------------------------------------
        -- Playlists
        -----------------------------------------
        ActivatePlaylist a ->
            Playlists.activate a

        UI.AddTracksToPlaylist a ->
            Playlists.addTracksToPlaylist a

        CreatePlaylist ->
            Playlists.create

        DeactivatePlaylist ->
            Playlists.deactivate

        DeletePlaylist a ->
            Playlists.delete a

        DeselectPlaylist ->
            Playlists.deselect

        ModifyPlaylist ->
            Playlists.modify

        MoveTrackInSelectedPlaylist a ->
            Playlists.moveTrackInSelectedPlaylist a

        SelectPlaylist a ->
            Playlists.select a

        SetPlaylistCreationContext a ->
            Playlists.setCreationContext a

        SetPlaylistModificationContext a b ->
            Playlists.setModificationContext a b

        ShowPlaylistListMenu a b ->
            Playlists.showListMenu a b

        -----------------------------------------
        -- Routing
        -----------------------------------------
        ChangeUrlUsingPage a ->
            Common.changeUrlUsingPage a

        LinkClicked a ->
            Routing.linkClicked a

        PageChanged a ->
            Routing.transition a

        UrlChanged a ->
            Routing.urlChanged a

        -----------------------------------------
        -- Services
        -----------------------------------------
        GotLastFmSession a ->
            Services.gotLastFmSession a

        Scrobble a ->
            Services.scrobble a

        -----------------------------------------
        -- Tracks (TODO: Move)
        -----------------------------------------
        DownloadTracksFinished ->
            Tracks.downloadTracksFinished

        FailedToStoreTracksInCache a ->
            Tracks.failedToStoreTracksInCache a

        FinishedStoringTracksInCache a ->
            Tracks.finishedStoringTracksInCache a

        -----------------------------------------
        -- User
        -----------------------------------------
        ImportFile a ->
            User.importFile a

        ImportJson a ->
            User.importJson a

        LoadEnclosedUserData a ->
            User.loadEnclosedUserData a

        LoadHypaethralUserData a ->
            User.loadHypaethralUserData a

        UI.SaveEnclosedUserData ->
            User.saveEnclosedUserData

        -----------------------------------------
        -- ⚗️ Adjunct
        -----------------------------------------
        KeyboardMsg a ->
            Adjunct.keyboardInput a

        -----------------------------------------
        -- 🦉 Nested
        -----------------------------------------
        AuthenticationMsg a ->
            Authentication.update a

        QueueMsg a ->
            Queue.update a

        SourcesMsg a ->
            Sources.update a

        TracksMsg a ->
            Tracks.update a

        -----------------------------------------
        -- 📭 Other
        -----------------------------------------
        SetCurrentTime a ->
            Other.setCurrentTime a

        SetIsOnline a ->
            Other.setIsOnline a



-- 📰


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fromAlien Alien.translate

        -----------------------------------------
        -- Audio
        -----------------------------------------
        , Ports.noteProgress NoteProgress
        , Ports.requestPlayPause (always TogglePlay)
        , Ports.requestStop (always Stop)
        , Ports.setAudioDuration SetAudioDuration
        , Ports.setAudioHasStalled SetAudioHasStalled
        , Ports.setAudioIsLoading SetAudioIsLoading
        , Ports.setAudioIsPlaying SetAudioIsPlaying
        , Ports.setAudioPosition SetAudioPosition

        -----------------------------------------
        -- Backdrop
        -----------------------------------------
        , Ports.setAverageBackgroundColor ExtractedBackdropColor

        -----------------------------------------
        -- Interface
        -----------------------------------------
        , Browser.Events.onResize Interface.onResize
        , Ports.indicateTouchDevice (\_ -> SetIsTouchDevice True)
        , Ports.preferredColorSchemaChanged PreferredColorSchemaChanged
        , Ports.showErrorNotification (Notifications.error >> ShowNotification)
        , Ports.showStickyErrorNotification (Notifications.stickyError >> ShowNotification)

        -----------------------------------------
        -- Queue
        -----------------------------------------
        , Ports.activeQueueItemEnded (QueueMsg << always Queue.Shift)
        , Ports.requestNext (\_ -> QueueMsg Queue.Shift)
        , Ports.requestPrevious (\_ -> QueueMsg Queue.Rewind)

        -----------------------------------------
        -- Services
        -----------------------------------------
        , Ports.scrobble Scrobble

        -----------------------------------------
        -- Tracks
        -----------------------------------------
        , Ports.downloadTracksFinished (\_ -> DownloadTracksFinished)

        -----------------------------------------
        -- 📭 Et Cetera
        -----------------------------------------
        , Ports.setIsOnline SetIsOnline
        , Sub.map KeyboardMsg Keyboard.subscriptions
        , Time.every (60 * 1000) SetCurrentTime
        ]
