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
import Json.Decode
import Keyboard
import LastFm
import Maybe.Extra as Maybe
import Notifications
import Playlists.Encoding as Playlists
import Queue
import Return2 exposing (..)
import Return3
import Sources
import Sources.Encoding as Sources
import Task
import Time
import Tracks
import Tracks.Encoding as Tracks
import UI.Adjunct as Adjunct
import UI.Alfred.State as Alfred
import UI.Audio.State as Audio
import UI.Audio.Types as Audio
import UI.Authentication as Authentication
import UI.Authentication.ContextMenu as Authentication
import UI.Authentication.State as Authentication
import UI.Backdrop as Backdrop
import UI.Common.State as Common
import UI.Equalizer.State as Equalizer
import UI.Equalizer.View as Equalizer
import UI.EtCetera.State as EtCetera
import UI.Interface.State as Interface
import UI.Page as Page
import UI.Playlists as Playlists
import UI.Playlists.ContextMenu as Playlists
import UI.Playlists.State as Playlists
import UI.Ports as Ports
import UI.Queue as Queue
import UI.Queue.ContextMenu as Queue
import UI.Reply as Reply exposing (Reply(..))
import UI.Reply.Translate as Reply
import UI.Routing.State as Routing
import UI.Services.State as Services
import UI.Sources as Sources
import UI.Sources.ContextMenu as Sources
import UI.Tracks as Tracks
import UI.Tracks.ContextMenu as Tracks
import UI.Tracks.State as Tracks
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


init : Flags -> Url -> Nav.Key -> Return Model Msg
init flags url key =
    let
        rewrittenUrl =
            Page.rewriteUrl url

        maybePage =
            Page.fromUrl rewrittenUrl

        page =
            Maybe.withDefault Page.Index maybePage
    in
    { alfred = Nothing
    , contextMenu = Nothing
    , confirmation = Nothing
    , currentTime = Time.millisToPosix flags.initialTime
    , darkMode = flags.darkMode
    , downloading = Nothing
    , focusedOnInput = False
    , isDragging = False
    , isLoading = True
    , isOnline = flags.isOnline
    , isTouchDevice = False
    , isUpgrading = flags.upgrade
    , lastFm = LastFm.initialModel
    , navKey = key
    , notifications = []
    , page = page
    , pressedKeys = []
    , processAutomatically = True
    , url = url
    , viewport = flags.viewport

    -- Children
    -----------
    , authentication = Authentication.initialModel url
    , backdrop = Backdrop.initialModel
    , equalizer = Equalizer.initialModel
    , playlists = Playlists.initialModel
    , queue = Queue.initialModel
    , sources = Sources.initialModel
    , tracks = Tracks.initialModel

    -- Parts
    --------
    , audio = Audio.initialModel

    -- Debouncing
    -------------
    , debounce =
        0.25
            |> Debouncer.fromSeconds
            |> Debouncer.debounce
            |> Debouncer.toDebouncer
    }
        |> update
            (PageChanged page)
        |> addCommand
            (if Maybe.isNothing maybePage then
                Routing.resetUrl key url page

             else
                Cmd.none
            )
        |> addCommand
            (Task.perform SetCurrentTime Time.now)



-- 📣


update : Msg -> Model -> Return Model Msg
update msg =
    case msg of
        Bypass ->
            return

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
        -- Authentication
        -----------------------------------------
        AuthenticationBootFailure a ->
            Authentication.authenticationBootFailure a

        MissingSecretKey a ->
            Authentication.missingSecretKey a

        NotAuthenticated ->
            Authentication.notAuthenticated

        RemoteStorageWebfinger a b ->
            Authentication.remoteStorageWebfinger a b

        -----------------------------------------
        -- Children
        -----------------------------------------
        AuthenticationMsg sub ->
            \model ->
                Return3.wieldNested
                    Reply.translate
                    { mapCmd = AuthenticationMsg
                    , mapModel = \child -> { model | authentication = child }
                    , update = Authentication.update
                    }
                    { model = model.authentication
                    , msg = sub
                    }

        BackdropMsg sub ->
            \model ->
                Return3.wieldNested
                    Reply.translate
                    { mapCmd = BackdropMsg
                    , mapModel = \child -> { model | backdrop = child }
                    , update = Backdrop.update
                    }
                    { model = model.backdrop
                    , msg = sub
                    }

        PlaylistsMsg sub ->
            \model ->
                Return3.wieldNested
                    Reply.translate
                    { mapCmd = PlaylistsMsg
                    , mapModel = \child -> { model | playlists = child }
                    , update = Playlists.update
                    }
                    { model = model.playlists
                    , msg = sub
                    }

        QueueMsg sub ->
            \model ->
                Return3.wieldNested
                    Reply.translate
                    { mapCmd = QueueMsg
                    , mapModel = \child -> { model | queue = child }
                    , update = Queue.update
                    }
                    { model = model.queue
                    , msg = sub
                    }

        SourcesMsg sub ->
            \model ->
                Return3.wieldNested
                    Reply.translate
                    { mapCmd = SourcesMsg
                    , mapModel = \child -> { model | sources = child }
                    , update = Sources.update
                    }
                    { model = model.sources
                    , msg = sub
                    }

        TracksMsg sub ->
            \model ->
                Return3.wieldNested
                    Reply.translate
                    { mapCmd = TracksMsg
                    , mapModel = \child -> { model | tracks = child }
                    , update = Tracks.update
                    }
                    { model = model.tracks
                    , msg = sub
                    }

        -----------------------------------------
        -- Equalizer
        -----------------------------------------
        ActivateKnob a b ->
            Equalizer.organize (Equalizer.activateKnob a b)

        AdjustKnob a ->
            Equalizer.organize (Equalizer.adjustKnob a)

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
            Interface.toggleLoadingScreen a

        -----------------------------------------
        -- Playlists
        -----------------------------------------
        UI.AddTracksToPlaylist a ->
            Playlists.addTracksToPlaylist a

        -----------------------------------------
        -- Routing
        -----------------------------------------
        ChangeUrlUsingPage a ->
            Routing.changeUrlUsingPage a

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
        -- Tracks
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
        -- 🦉 Adjunct
        -----------------------------------------
        KeyboardMsg a ->
            Adjunct.keyboardInput a

        -----------------------------------------
        -- 📭 Et Cetera
        -----------------------------------------
        SetCurrentTime a ->
            EtCetera.setCurrentTime a

        SetIsOnline a ->
            EtCetera.setIsOnline a

        -----------------------------------------
        -- TODO
        -----------------------------------------
        Audio a ->
            Audio.update a



-- 📰


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Audio.subscriptions model
        , Ports.fromAlien alien

        -----------------------------------------
        -- Backdrop
        -----------------------------------------
        , Ports.setAverageBackgroundColor (Backdrop.BackgroundColor >> BackdropMsg)

        -----------------------------------------
        -- Interface
        -----------------------------------------
        , Ports.indicateTouchDevice (\_ -> SetIsTouchDevice True)
        , Ports.preferredColorSchemaChanged PreferredColorSchemaChanged
        , Ports.showErrorNotification (Notifications.error >> ShowNotification)
        , Ports.showStickyErrorNotification (Notifications.stickyError >> ShowNotification)

        -----------------------------------------
        -- Queue
        -----------------------------------------
        , Ports.activeQueueItemEnded (QueueMsg << always Queue.Shift)
        , Ports.requestNext <| always (QueueMsg Queue.Shift)
        , Ports.requestPrevious <| always (QueueMsg Queue.Rewind)

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

        -- Resize
        ---------
        , Browser.Events.onResize
            (\w h ->
                ( w, h )
                    |> ResizedWindow
                    |> Debouncer.provideInput
                    |> Debounce
            )
        ]


alien : Alien.Event -> Msg
alien event =
    case event.error of
        Nothing ->
            translateAlienData event

        Just err ->
            translateAlienError event err


translateAlienData : Alien.Event -> Msg
translateAlienData event =
    case
        Alien.tagFromString event.tag
    of
        Just Alien.AddTracks ->
            TracksMsg (Tracks.Add event.data)

        Just Alien.AuthMethod ->
            -- My brain told me which auth method we're using,
            -- so we can tell the user in the UI.
            case decodeMethod event.data of
                Just method ->
                    AuthenticationMsg (Authentication.SignedIn method)

                Nothing ->
                    Bypass

        Just Alien.FinishedProcessingSource ->
            event.data
                |> Json.Decode.decodeValue Json.Decode.string
                |> Result.map (Sources.FinishedProcessingSource >> SourcesMsg)
                |> Result.withDefault Bypass

        Just Alien.FinishedProcessingSources ->
            SourcesMsg Sources.FinishedProcessing

        Just Alien.HideLoadingScreen ->
            UI.ToggleLoadingScreen Off

        Just Alien.ImportLegacyData ->
            "Imported data successfully!"
                |> Notifications.success
                |> ShowNotification

        Just Alien.LoadEnclosedUserData ->
            LoadEnclosedUserData event.data

        Just Alien.LoadHypaethralUserData ->
            LoadHypaethralUserData event.data

        Just Alien.MissingSecretKey ->
            MissingSecretKey event.data

        Just Alien.NotAuthenticated ->
            NotAuthenticated

        Just Alien.RemoveTracksByPath ->
            TracksMsg (Tracks.RemoveByPaths event.data)

        Just Alien.ReportProcessingError ->
            SourcesMsg (Sources.ReportProcessingError event.data)

        Just Alien.ReportProcessingProgress ->
            SourcesMsg (Sources.ReportProcessingProgress event.data)

        Just Alien.SearchTracks ->
            TracksMsg (Tracks.SetSearchResults event.data)

        Just Alien.StoreTracksInCache ->
            case
                Json.Decode.decodeValue
                    (Json.Decode.list Json.Decode.string)
                    event.data
            of
                Ok list ->
                    FinishedStoringTracksInCache list

                Err err ->
                    showErrorNotification (Json.Decode.errorToString err)

        Just Alien.UpdateSourceData ->
            SourcesMsg (Sources.UpdateSourceData event.data)

        _ ->
            Bypass


translateAlienError : Alien.Event -> String -> Msg
translateAlienError event err =
    case
        Alien.tagFromString event.tag
    of
        Just Alien.AuthAnonymous ->
            AuthenticationBootFailure err

        Just Alien.AuthBlockstack ->
            AuthenticationBootFailure err

        Just Alien.AuthDropbox ->
            AuthenticationBootFailure err

        Just Alien.AuthIpfs ->
            AuthenticationBootFailure err

        Just Alien.AuthRemoteStorage ->
            AuthenticationBootFailure err

        Just Alien.AuthTextile ->
            AuthenticationBootFailure err

        Just Alien.StoreTracksInCache ->
            case
                Json.Decode.decodeValue
                    (Json.Decode.list Json.Decode.string)
                    event.data
            of
                Ok trackIds ->
                    FailedToStoreTracksInCache trackIds

                Err _ ->
                    showErrorNotification err

        _ ->
            showErrorNotification err



-- ⚗️


showErrorNotification : String -> Msg
showErrorNotification =
    Notifications.error >> ShowNotification
