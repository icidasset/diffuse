module UI exposing (main)

import Alfred exposing (Alfred)
import Alien
import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Chunky exposing (..)
import Common exposing (Switch(..))
import Conditional exposing (..)
import ContextMenu exposing (ContextMenu)
import Css exposing (url)
import Css.Classes as C
import Debouncer.Basic as Debouncer
import Dict
import Dict.Ext as Dict
import File
import File.Download
import File.Select
import Html exposing (Html, section)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (on, onClick)
import Html.Events.Extra.Pointer as Pointer
import Html.Lazy as Lazy
import Json.Decode
import Json.Encode
import LastFm
import List.Ext as List
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Lens as Lens
import Notifications
import Playlists.Encoding as Playlists
import Process
import Queue
import Return2 exposing (..)
import Return3
import Settings
import Sources
import Sources.Encoding as Sources
import Sources.Services.Dropbox
import Sources.Services.Google
import String.Ext as String
import Task
import Time
import Tracks
import Tracks.Encoding as Tracks
import UI.Alfred as Alfred
import UI.Audio.State as Audio
import UI.Audio.Types as Audio
import UI.Authentication as Authentication
import UI.Authentication.ContextMenu as Authentication
import UI.Backdrop as Backdrop
import UI.Common.State exposing (showNotification, showNotificationWithModel)
import UI.Console
import UI.ContextMenu
import UI.Demo as Demo
import UI.Equalizer as Equalizer
import UI.Interface.State as Interface
import UI.Interface.Types as Interface
import UI.Navigation as Navigation
import UI.Notifications
import UI.Page as Page
import UI.Playlists as Playlists
import UI.Playlists.Alfred
import UI.Playlists.ContextMenu as Playlists
import UI.Playlists.Directory
import UI.Ports as Ports
import UI.Queue as Queue
import UI.Queue.ContextMenu as Queue
import UI.Reply as Reply exposing (Reply(..))
import UI.Reply.Translate as Reply
import UI.Routing.State as Routing
import UI.Services.State as Services
import UI.Settings as Settings
import UI.Settings.Page
import UI.Sources as Sources
import UI.Sources.ContextMenu as Sources
import UI.Sources.Form
import UI.Sources.Page
import UI.Svg.Elements
import UI.Tracks as Tracks
import UI.Tracks.ContextMenu as Tracks
import UI.Tracks.Scene.List
import UI.Tracks.State as Tracks
import UI.Types as UI exposing (..)
import UI.User.State as User
import UI.View exposing (view)
import Url exposing (Protocol(..), Url)
import Url.Ext as Url
import User.Layer exposing (..)
import User.Layer.Methods.RemoteStorage as RemoteStorage



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
    { contextMenu = Nothing
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
    , alfred = Alfred.initialModel
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
            (Task.perform
                (Interface.SetCurrentTime >> Interface)
                Time.now
            )



-- 📣


update : Msg -> Model -> Return Model Msg
update msg model =
    case msg of
        Bypass ->
            return model

        Reply reply ->
            Reply.translate reply model

        --
        Audio a ->
            Audio.update a model

        Interface a ->
            Interface.update a model

        -----------------------------------------
        -- Authentication
        -----------------------------------------
        AuthenticationBootFailure err ->
            model
                |> showNotification (Notifications.error err)
                |> andThen (Reply.translate LoadDefaultBackdrop)

        MissingSecretKey json ->
            "There seems to be existing data that's encrypted, I will need the passphrase (ie. encryption key) to continue."
                |> Notifications.error
                |> showNotificationWithModel model
                |> andThen (Reply.translate <| Reply.LoadDefaultBackdrop)
                |> andThen (Reply.translate <| Reply.ToggleLoadingScreen Off)

        NotAuthenticated ->
            -- This is the message we get when the app initially
            -- finds out we're not authenticated.
            andThen
                (update <| BackdropMsg Backdrop.Default)
                (if model.isUpgrading then
                    """
                    Thank you for using Diffuse V1!
                    If you want to import your old data,
                    please pick the storage method you used before and
                    go to the [import page](#/settings/import-export).
                    """
                        |> Notifications.stickySuccess
                        |> showNotificationWithModel { model | isUpgrading = False }

                 else
                    return model
                )

        RemoteStorageWebfinger remoteStorage (Ok oauthOrigin) ->
            let
                origin =
                    Common.urlOrigin model.url
            in
            remoteStorage
                |> RemoteStorage.oauthAddress
                    { oauthOrigin = oauthOrigin
                    , origin = origin
                    }
                |> Nav.load
                |> returnWithModel model

        RemoteStorageWebfinger _ (Err _) ->
            RemoteStorage.webfingerError
                |> Notifications.error
                |> showNotificationWithModel model

        -----------------------------------------
        -- Children
        -----------------------------------------
        AlfredMsg sub ->
            Return3.wieldNested
                Reply.translate
                { mapCmd = AlfredMsg
                , mapModel = \child -> { model | alfred = child }
                , update = Alfred.update
                }
                { model = model.alfred
                , msg = sub
                }

        AuthenticationMsg sub ->
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
            Return3.wieldNested
                Reply.translate
                { mapCmd = BackdropMsg
                , mapModel = \child -> { model | backdrop = child }
                , update = Backdrop.update
                }
                { model = model.backdrop
                , msg = sub
                }

        EqualizerMsg sub ->
            Return3.wieldNested
                Reply.translate
                { mapCmd = EqualizerMsg
                , mapModel = \child -> { model | equalizer = child }
                , update = Equalizer.update
                }
                { model = model.equalizer
                , msg = sub
                }

        PlaylistsMsg sub ->
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
        -- Routing
        -----------------------------------------
        ChangeUrlUsingPage a ->
            Routing.changeUrlUsingPage a model

        LinkClicked a ->
            Routing.linkClicked a model

        PageChanged a ->
            Routing.transition a model

        UrlChanged a ->
            Routing.urlChanged a model

        -----------------------------------------
        -- Services
        -----------------------------------------
        GotLastFmSession a ->
            Services.gotLastFmSession a model

        Scrobble a ->
            Services.scrobble a model

        -----------------------------------------
        -- Tracks
        -----------------------------------------
        DownloadTracksFinished ->
            Tracks.downloadTracksFinished model

        FailedToStoreTracksInCache a ->
            Tracks.failedToStoreTracksInCache a model

        FinishedStoringTracksInCache a ->
            Tracks.finishedStoringTracksInCache a model

        -----------------------------------------
        -- User
        -----------------------------------------
        ImportFile a ->
            User.importFile a model

        ImportJson a ->
            User.importJson a model

        LoadEnclosedUserData a ->
            User.loadEnclosedUserData a model

        LoadHypaethralUserData a ->
            User.loadHypaethralUserData a model



-- 📰


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fromAlien alien

        -- Queue
        --------
        , Ports.activeQueueItemEnded (QueueMsg << always Queue.Shift)
        , Ports.requestNext <| always (QueueMsg Queue.Shift)
        , Ports.requestPrevious <| always (QueueMsg Queue.Rewind)

        -- Children
        -----------
        , ifThenElse
            (Maybe.isJust model.alfred.instance)
            (Sub.map AlfredMsg <| Alfred.subscriptions model.alfred)
            Sub.none

        --
        , Ports.downloadTracksFinished (\_ -> DownloadTracksFinished)
        , Ports.scrobble Scrobble
        , Ports.setAverageBackgroundColor (Backdrop.BackgroundColor >> BackdropMsg)

        --
        , Audio.subscriptions model
        , Interface.subscriptions model
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
            Interface (Interface.ToggleLoadingScreen Off)

        Just Alien.ImportLegacyData ->
            "Imported data successfully!"
                |> Notifications.success
                |> Interface.ShowNotification
                |> Interface

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
                    err
                        |> Json.Decode.errorToString
                        |> Notifications.error
                        |> Interface.ShowNotification
                        |> Interface

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
                    err
                        |> Notifications.error
                        |> Interface.ShowNotification
                        |> Interface

        _ ->
            err
                |> Notifications.error
                |> Interface.ShowNotification
                |> Interface
