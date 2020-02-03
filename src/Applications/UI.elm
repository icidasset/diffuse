module UI exposing (main)

import Alfred exposing (Alfred)
import Alien
import Browser
import Browser.Dom
import Browser.Events
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
import Keyboard
import LastFm
import List.Ext as List
import List.Extra as List
import Maybe.Extra as Maybe
import Notifications exposing (Notification)
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
import UI.Console
import UI.ContextMenu
import UI.Demo as Demo
import UI.DnD as DnD
import UI.Equalizer as Equalizer
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
import UI.Types exposing (..)
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

    -- Audio
    --------
    , audioDuration = 0
    , audioHasStalled = False
    , audioIsLoading = False
    , audioIsPlaying = False
    , audioPosition = 0

    --
    , progress = Dict.empty
    , rememberProgress = True

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
                resetUrl key url page

             else
                Cmd.none
            )
        |> addCommand
            (Task.perform SetCurrentTime Time.now)



-- 📣


update : Msg -> Model -> Return Model Msg
update msg model =
    case msg of
        Bypass ->
            return model

        Reply reply ->
            translateReply reply model

        --
        Blur ->
            return { model | focusedOnInput = False }

        Debounce debouncerMsg ->
            Return3.wieldNested
                update
                { mapCmd = Debounce
                , mapModel = \child -> { model | debounce = child }
                , update = \m -> Debouncer.update m >> Return3.fromDebouncer
                }
                { model = model.debounce
                , msg = debouncerMsg
                }

        DownloadTracksFinished ->
            case model.downloading of
                Just { notificationId } ->
                    { id = notificationId }
                        |> DismissNotification
                        |> translateReplyWithModel { model | downloading = Nothing }

                Nothing ->
                    return model

        FocusedOnInput ->
            return { model | focusedOnInput = True }

        HideOverlay ->
            hideOverlay model

        KeyboardMsg subMsg ->
            (\m ->
                let
                    skip =
                        return m

                    authenticated =
                        case model.authentication of
                            Authentication.Authenticated _ ->
                                True

                            _ ->
                                False
                in
                if m.focusedOnInput || not authenticated then
                    -- Stop here if using input or not authenticated
                    skip

                else
                    case m.pressedKeys of
                        [ Keyboard.Escape ] ->
                            hideOverlay m

                        [ Keyboard.ArrowLeft ] ->
                            translateReply RewindQueue m

                        [ Keyboard.ArrowRight ] ->
                            translateReply ShiftQueue m

                        [ Keyboard.ArrowUp ] ->
                            translateReply (Seek <| (m.audioPosition - 10) / m.audioDuration) m

                        [ Keyboard.ArrowDown ] ->
                            translateReply (Seek <| (m.audioPosition + 10) / m.audioDuration) m

                        [ Keyboard.Character "N" ] ->
                            translateReply ScrollToNowPlaying m

                        [ Keyboard.Character "P" ] ->
                            translateReply TogglePlayPause m

                        [ Keyboard.Character "R" ] ->
                            translateReply ToggleRepeat m

                        [ Keyboard.Character "S" ] ->
                            translateReply ToggleShuffle m

                        _ ->
                            skip
            )
                { model | pressedKeys = Keyboard.update subMsg model.pressedKeys }

        LoadEnclosedUserData json ->
            model
                |> importEnclosed json
                |> Return3.wield translateReply

        LoadHypaethralUserData json ->
            model
                |> importHypaethral json
                |> Return3.wield translateReply
                |> andThen
                    (\m ->
                        case Url.action m.url of
                            [ "authenticate", "lastfm" ] ->
                                { authenticating = True
                                , sessionKey = Nothing
                                }
                                    |> (\n -> { m | lastFm = n })
                                    |> returnWithCommand (LastFm.authenticationCommand GotLastFmSession m.url)

                            _ ->
                                return m
                    )
                |> andThen
                    (\m ->
                        if m.isUpgrading then
                            """
                            Thank you for using Diffuse V1!
                            If you want to import your old data,
                            please go to the [import page](#/settings/import-export).
                            """
                                |> ShowStickySuccessNotification
                                |> translateReplyWithModel m

                        else
                            return m
                    )
                |> andThen
                    (\m ->
                        if m.processAutomatically then
                            m.sources
                                |> Sources.sourcesToProcess
                                |> ProcessSources
                                |> translateReplyWithModel m

                        else
                            return m
                    )

        PreferredColorSchemaChanged { dark } ->
            return { model | darkMode = dark }

        RemoveQueueSelection ->
            let
                queue =
                    model.queue
            in
            ( { model
                | queue = { queue | selection = Nothing }
              }
            , Cmd.none
            )

        RemoveTrackSelection ->
            let
                tracks =
                    model.tracks
            in
            ( { model
                | tracks = { tracks | selectedTrackIndexes = [] }
              }
            , Cmd.none
            )

        ResizedWindow ( width, height ) ->
            ( { model
                | contextMenu = Nothing
                , viewport = { height = toFloat height, width = toFloat width }
              }
              --
            , Cmd.none
            )

        SetCurrentTime time ->
            let
                sources =
                    model.sources
            in
            ( { model
                | currentTime = time
                , sources = { sources | currentTime = time }
              }
              --
            , Cmd.none
            )

        SetIsOnline False ->
            -- The app went offline, cache everything
            -- (if caching is supported).
            ( { model | isOnline = False }
            , case model.authentication of
                Authentication.Authenticated (Dropbox _) ->
                    Ports.toBrain (Alien.trigger Alien.SyncHypaethralData)

                Authentication.Authenticated (RemoteStorage _) ->
                    Ports.toBrain (Alien.trigger Alien.SyncHypaethralData)

                _ ->
                    Cmd.none
            )

        SetIsOnline True ->
            -- We're caching the user's data in the browser while offline.
            -- If we're back online again, sync all the user's data.
            (case model.authentication of
                Authentication.Authenticated (Dropbox _) ->
                    syncHypaethralData

                Authentication.Authenticated (RemoteStorage _) ->
                    syncHypaethralData

                _ ->
                    return
            )
                { model | isOnline = True }

        SetIsTouchDevice bool ->
            return { model | isTouchDevice = bool }

        ShowNotification notification ->
            showNotification notification model

        StoppedDragging ->
            let
                notDragging =
                    { model | isDragging = False }
            in
            -- Depending on where we stopped dragging something,
            -- do the appropriate thing.
            case model.page of
                Page.Queue _ ->
                    DnD.stoppedDragging
                        |> Queue.DragMsg
                        |> QueueMsg
                        |> updateWithModel notDragging

                Page.Index ->
                    case model.tracks.scene of
                        Tracks.List ->
                            DnD.stoppedDragging
                                |> UI.Tracks.Scene.List.DragAndDropMsg
                                |> Tracks.ListSceneMsg
                                |> TracksMsg
                                |> updateWithModel notDragging

                _ ->
                    return notDragging

        UI.Types.ToggleLoadingScreen On ->
            return { model | isLoading = True }

        UI.Types.ToggleLoadingScreen Off ->
            return { model | isLoading = False }

        -----------------------------------------
        -- Authentication
        -----------------------------------------
        AuthenticationBootFailure err ->
            model
                |> showNotification (Notifications.error err)
                |> andThen (translateReply LoadDefaultBackdrop)

        MissingSecretKey json ->
            "There seems to be existing data that's encrypted, I will need the passphrase (ie. encryption key) to continue."
                |> Notifications.error
                |> showNotificationWithModel model
                |> andThen (translateReply <| Reply.LoadDefaultBackdrop)
                |> andThen (translateReply <| Reply.ToggleLoadingScreen Off)

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
                translateReply
                { mapCmd = AlfredMsg
                , mapModel = \child -> { model | alfred = child }
                , update = Alfred.update
                }
                { model = model.alfred
                , msg = sub
                }

        AuthenticationMsg sub ->
            Return3.wieldNested
                translateReply
                { mapCmd = AuthenticationMsg
                , mapModel = \child -> { model | authentication = child }
                , update = Authentication.update
                }
                { model = model.authentication
                , msg = sub
                }

        BackdropMsg sub ->
            Return3.wieldNested
                translateReply
                { mapCmd = BackdropMsg
                , mapModel = \child -> { model | backdrop = child }
                , update = Backdrop.update
                }
                { model = model.backdrop
                , msg = sub
                }

        EqualizerMsg sub ->
            Return3.wieldNested
                translateReply
                { mapCmd = EqualizerMsg
                , mapModel = \child -> { model | equalizer = child }
                , update = Equalizer.update
                }
                { model = model.equalizer
                , msg = sub
                }

        PlaylistsMsg sub ->
            Return3.wieldNested
                translateReply
                { mapCmd = PlaylistsMsg
                , mapModel = \child -> { model | playlists = child }
                , update = Playlists.update
                }
                { model = model.playlists
                , msg = sub
                }

        QueueMsg sub ->
            Return3.wieldNested
                translateReply
                { mapCmd = QueueMsg
                , mapModel = \child -> { model | queue = child }
                , update = Queue.update
                }
                { model = model.queue
                , msg = sub
                }

        SourcesMsg sub ->
            Return3.wieldNested
                translateReply
                { mapCmd = SourcesMsg
                , mapModel = \child -> { model | sources = child }
                , update = Sources.update
                }
                { model = model.sources
                , msg = sub
                }

        TracksMsg sub ->
            Return3.wieldNested
                translateReply
                { mapCmd = TracksMsg
                , mapModel = \child -> { model | tracks = child }
                , update = Tracks.update
                }
                { model = model.tracks
                , msg = sub
                }

        -----------------------------------------
        -- Import / Export
        -----------------------------------------
        Import file ->
            250
                |> Process.sleep
                |> Task.andThen (\_ -> File.toString file)
                |> Task.perform ImportJson
                |> returnWithModel { model | isLoading = True }

        ImportJson json ->
            json
                -- Load data on main thread (this app)
                |> Json.Decode.decodeString Json.Decode.value
                |> Result.withDefault Json.Encode.null
                |> (\j -> importHypaethral j model)
                |> Return3.wield translateReply
                -- Show notification
                |> andThen
                    ("Imported data successfully!"
                        |> Notifications.success
                        |> showNotification
                    )
                -- Clear tracks cache
                |> andThen (translateReply ClearTracksCache)
                -- Redirect to index page
                |> andThen (update <| ChangeUrlUsingPage Page.Index)
                -----------------------------
                -- Save all the imported data
                -----------------------------
                |> saveAllHypaethralData

        -----------------------------------------
        -- Last.fm
        -----------------------------------------
        GotLastFmSession (Ok sessionKey) ->
            { model | lastFm = LastFm.gotSessionKey sessionKey model.lastFm }
                |> showNotification
                    (Notifications.success "Connected successfully with Last.fm")
                |> andThen
                    (translateReply SaveSettings)

        GotLastFmSession (Err _) ->
            showNotification
                (Notifications.stickyError "Could not connect with Last.fm")
                { model | lastFm = LastFm.failedToAuthenticate model.lastFm }

        Scrobble { duration, timestamp, trackId } ->
            case model.tracks.nowPlaying of
                Just ( _, track ) ->
                    if trackId == track.id then
                        ( model
                        , LastFm.scrobble model.lastFm
                            { duration = duration
                            , msg = Bypass
                            , timestamp = timestamp
                            , track = track
                            }
                        )

                    else
                        return model

                Nothing ->
                    return model

        -----------------------------------------
        -- Page Transitions
        -----------------------------------------
        -- Sources.NewThroughRedirect
        -----------------------------
        PageChanged (Page.Sources (UI.Sources.Page.NewThroughRedirect service args)) ->
            let
                ( sources, form, defaultContext ) =
                    ( model.sources
                    , model.sources.form
                    , UI.Sources.Form.defaultContext
                    )
            in
            { defaultContext
                | data =
                    case service of
                        Sources.Dropbox ->
                            Sources.Services.Dropbox.authorizationSourceData args

                        Sources.Google ->
                            Sources.Services.Google.authorizationSourceData args

                        _ ->
                            defaultContext.data
                , service =
                    service
            }
                |> (\c -> { form | context = c, step = UI.Sources.Form.By })
                |> (\f -> { sources | form = f })
                |> (\s -> { model | sources = s })
                |> return

        -- Sources.Edit
        ---------------
        PageChanged (Page.Sources (UI.Sources.Page.Edit sourceId)) ->
            loadSourceForForm model sourceId

        -- Sources.Rename
        -----------------
        PageChanged (Page.Sources (UI.Sources.Page.Rename sourceId)) ->
            loadSourceForForm model sourceId

        --
        PageChanged _ ->
            return model

        -----------------------------------------
        -- Tracks Cache
        -----------------------------------------
        FailedToStoreTracksInCache trackIds ->
            model
                |> updateTracksModel
                    (\m -> { m | cachingInProgress = List.without trackIds m.cachingInProgress })
                |> showNotification
                    (Notifications.error "Failed to store track in cache")

        FinishedStoringTracksInCache trackIds ->
            model
                |> updateTracksModel
                    (\t ->
                        { t
                            | cached = t.cached ++ trackIds
                            , cachingInProgress = List.without trackIds t.cachingInProgress
                        }
                    )
                |> (\m ->
                        -- When a context menu of a track is open,
                        -- it should be "rerendered" in case
                        -- the track is no longer being downloaded.
                        case m.contextMenu of
                            Just contextMenu ->
                                let
                                    isTrackContextMenu =
                                        ContextMenu.anyItem
                                            (.label >> (==) "Downloading ...")
                                            contextMenu

                                    coordinates =
                                        ContextMenu.coordinates contextMenu
                                in
                                if isTrackContextMenu then
                                    m.tracks.collection.harvested
                                        |> List.pickIndexes m.tracks.selectedTrackIndexes
                                        |> ShowTracksContextMenu coordinates { alt = False }
                                        |> translateReplyWithModel m

                                else
                                    return m

                            Nothing ->
                                return m
                   )
                |> andThen (update <| TracksMsg Tracks.Harvest)
                |> andThen (translateReply SaveEnclosedUserData)

        -----------------------------------------
        -- URL
        -----------------------------------------
        ChangeUrlUsingPage page ->
            page
                |> Page.toString
                |> Nav.pushUrl model.navKey
                |> returnWithModel model

        LinkClicked (Browser.Internal urlWithFragment) ->
            let
                url =
                    if urlWithFragment.fragment == Just "/" then
                        { urlWithFragment | fragment = Nothing }

                    else
                        urlWithFragment
            in
            if url.path /= model.url.path then
                returnWithModel model (Nav.load url.path)

            else
                returnWithModel model (Nav.pushUrl model.navKey <| Url.toString url)

        LinkClicked (Browser.External href) ->
            returnWithModel model (Nav.load href)

        UrlChanged url ->
            let
                rewrittenUrl =
                    Page.rewriteUrl { url | query = Nothing }
            in
            case ( url.query, Page.fromUrl rewrittenUrl ) of
                ( Nothing, Just page ) ->
                    { model | page = page, url = url }
                        |> return
                        |> andThen (update <| PageChanged page)

                ( Just _, Just page ) ->
                    returnWithModel model (resetUrl model.navKey url page)

                _ ->
                    returnWithModel model (resetUrl model.navKey url Page.Index)

        -----------------------------------------
        -- TARGET => Pieces
        -----------------------------------------
        Audio a ->
            Audio.update a model


updateTracksModel : (Tracks.Model -> Tracks.Model) -> Model -> Model
updateTracksModel fn model =
    { model | tracks = fn model.tracks }


updateWithModel : Model -> Msg -> Return Model Msg
updateWithModel model msg =
    update msg model



-- 📣  ░░  REPLIES


translateReply : Reply -> Model -> Return Model Msg
translateReply reply model =
    case reply of
        Shunt ->
            return model

        --
        CopyToClipboard string ->
            string
                |> Ports.copyToClipboard
                |> returnWithModel model

        GoToPage page ->
            page
                |> ChangeUrlUsingPage
                |> updateWithModel model

        StartedDragging ->
            return { model | isDragging = True }

        Reply.ToggleLoadingScreen Off ->
            return { model | isLoading = False }

        Reply.ToggleLoadingScreen On ->
            return { model | isLoading = True }

        -----------------------------------------
        -- Audio
        -----------------------------------------
        Seek percentage ->
            returnWithModel model (Ports.seek percentage)

        TogglePlayPause ->
            Audio.playPause model

        ToggleRememberProgress ->
            translateReply SaveSettings { model | rememberProgress = not model.rememberProgress }

        -----------------------------------------
        -- Authentication
        -----------------------------------------
        ExternalAuth Blockstack _ ->
            Alien.RedirectToBlockstackSignIn
                |> Alien.trigger
                |> Ports.toBrain
                |> returnWithModel model

        ExternalAuth (Dropbox _) _ ->
            [ ( "response_type", "token" )
            , ( "client_id", "te0c9pbeii8f8bw" )
            , ( "redirect_uri", Common.urlOrigin model.url ++ "?action=authenticate/dropbox" )
            ]
                |> Common.queryString
                |> String.append "https://www.dropbox.com/oauth2/authorize"
                |> Nav.load
                |> returnWithModel model

        ExternalAuth (RemoteStorage _) input ->
            input
                |> RemoteStorage.parseUserAddress
                |> Maybe.map
                    (RemoteStorage.webfingerRequest RemoteStorageWebfinger)
                |> Maybe.unwrap
                    (translateReply
                        (ShowErrorNotification RemoteStorage.userAddressError)
                        model
                    )
                    (returnWithModel model)

        ExternalAuth _ _ ->
            return model

        ImportLegacyData ->
            Alien.ImportLegacyData
                |> Alien.trigger
                |> Ports.toBrain
                |> returnWithModel model
                |> andThen
                    ("""
                     I'll try to import data from Diffuse version one.
                     If this was successful, you'll get a notification.
                     """
                        |> Notifications.warning
                        |> showNotification
                    )

        PingIpfsForAuth ->
            case model.url.protocol of
                Https ->
                    """
                    Unfortunately the local IPFS API doesn't work with HTTPS.
                    Install the [IPFS Companion](https://github.com/ipfs-shipyard/ipfs-companion#release-channel) browser extension to get around this issue
                    (and make sure it redirects to the local gateway).
                    """
                        |> Notifications.error
                        |> showNotificationWithModel model

                Http ->
                    Authentication.PingIpfs
                        |> AuthenticationMsg
                        |> updateWithModel model

        PingTextileForAuth ->
            Authentication.PingTextile
                |> AuthenticationMsg
                |> updateWithModel model

        ShowUpdateEncryptionKeyScreen authMethod ->
            authMethod
                |> Authentication.ShowUpdateEncryptionKeyScreen
                |> AuthenticationMsg
                |> updateWithModel model

        SignOut ->
            let
                { playlists, sources, tracks } =
                    model
            in
            { model
                | authentication = Authentication.Unauthenticated
                , playlists =
                    { playlists
                        | collection = []
                        , playlistToActivate = Maybe.map .name tracks.selectedPlaylist
                    }
                , queue =
                    Queue.initialModel
                , sources =
                    { sources
                        | collection = []
                        , isProcessing = []
                    }
                , tracks =
                    { tracks
                        | collection = Tracks.emptyCollection
                        , enabledSourceIds = []
                        , favourites = []
                        , hideDuplicates = Tracks.initialModel.hideDuplicates
                        , nowPlaying = Nothing
                        , searchResults = Nothing
                    }
            }
                |> update (BackdropMsg Backdrop.Default)
                |> addCommand (Ports.toBrain <| Alien.trigger Alien.SignOut)
                |> addCommand (Ports.toBrain <| Alien.trigger Alien.StopProcessing)
                |> addCommand (Ports.activeQueueItemChanged Nothing)
                |> addCommand (Nav.pushUrl model.navKey "#/")

        -----------------------------------------
        -- Context Menu
        -----------------------------------------
        ContextMenuConfirmation conf r ->
            { model | confirmation = Just conf }
                |> return
                |> andThen (translateReply r)

        ReplyViaContextMenu r ->
            case r of
                ContextMenuConfirmation _ _ ->
                    translateReply r model

                _ ->
                    translateReply r { model | contextMenu = Nothing }

        ShowMoreAuthenticationOptions coordinates ->
            return { model | contextMenu = Just (Authentication.moreOptionsMenu coordinates) }

        ShowPlaylistListMenu coordinates playlist ->
            return { model | contextMenu = Just (Playlists.listMenu playlist model.tracks.collection.identified model.confirmation coordinates) }

        ShowQueueFutureMenu coordinates { item, itemIndex } ->
            return { model | contextMenu = Just (Queue.futureMenu { cached = model.tracks.cached, cachingInProgress = model.tracks.cachingInProgress, itemIndex = itemIndex } item coordinates) }

        ShowQueueHistoryMenu coordinates { item } ->
            return { model | contextMenu = Just (Queue.historyMenu { cached = model.tracks.cached, cachingInProgress = model.tracks.cachingInProgress } item coordinates) }

        ShowSourceContextMenu coordinates source ->
            return { model | contextMenu = Just (Sources.sourceMenu source coordinates) }

        ShowTracksContextMenu coordinates { alt } tracks ->
            let
                menuDependencies =
                    { cached = model.tracks.cached
                    , cachingInProgress = model.tracks.cachingInProgress
                    , currentTime = model.currentTime
                    , selectedPlaylist = model.tracks.selectedPlaylist
                    , lastModifiedPlaylistName = model.playlists.lastModifiedPlaylist
                    , showAlternativeMenu = alt
                    , sources = model.sources.collection
                    }
            in
            return { model | contextMenu = Just (Tracks.trackMenu menuDependencies tracks coordinates) }

        ShowTracksViewMenu coordinates maybeGrouping ->
            return { model | contextMenu = Just (Tracks.viewMenu model.tracks.cachedOnly maybeGrouping coordinates) }

        -----------------------------------------
        -- Last.fm
        -----------------------------------------
        ConnectLastFm ->
            model.url
                |> Common.urlOrigin
                |> String.addSuffix "?action=authenticate/lastfm"
                |> Url.percentEncode
                |> String.append "&cb="
                |> String.append
                    (String.append
                        "http://www.last.fm/api/auth/?api_key="
                        LastFm.apiKey
                    )
                |> Nav.load
                |> returnWithModel model

        DisconnectLastFm ->
            translateReply
                SaveSettings
                { model | lastFm = LastFm.disconnect model.lastFm }

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        DismissNotification options ->
            options
                |> UI.Notifications.dismiss model.notifications
                |> mapModel (\n -> { model | notifications = n })
                |> mapCommand Reply

        RemoveNotification { id } ->
            model.notifications
                |> List.filter (Notifications.id >> (/=) id)
                |> (\n -> { model | notifications = n })
                |> return

        ShowErrorNotification string ->
            showNotificationWithModel model (Notifications.error string)

        ShowStickyErrorNotification string ->
            showNotificationWithModel model (Notifications.stickyError string)

        ShowStickyErrorNotificationWithCode string code ->
            showNotificationWithModel model (Notifications.errorWithCode string code [])

        ShowSuccessNotification string ->
            showNotificationWithModel model (Notifications.success string)

        ShowStickySuccessNotification string ->
            showNotificationWithModel model (Notifications.stickySuccess string)

        ShowWarningNotification string ->
            showNotificationWithModel model (Notifications.warning string)

        ShowStickyWarningNotification string ->
            showNotificationWithModel model (Notifications.stickyWarning string)

        -----------------------------------------
        -- Playlists
        -----------------------------------------
        ActivatePlaylist playlist ->
            playlist
                |> Tracks.SelectPlaylist
                |> TracksMsg
                |> updateWithModel model

        AddTracksToPlaylist { playlistName, tracks } ->
            let
                properPlaylistName =
                    String.trim playlistName

                playlistIndex =
                    List.findIndex
                        (\p -> p.autoGenerated == False && p.name == properPlaylistName)
                        model.playlists.collection

                playlistsModel =
                    model.playlists

                newCollection =
                    case playlistIndex of
                        Just idx ->
                            List.updateAt
                                idx
                                (\p -> { p | tracks = p.tracks ++ tracks })
                                playlistsModel.collection

                        Nothing ->
                            (::)
                                { autoGenerated = False
                                , name = properPlaylistName
                                , tracks = tracks
                                }
                                playlistsModel.collection

                newModel =
                    { playlistsModel
                        | collection = newCollection
                        , lastModifiedPlaylist = Just properPlaylistName
                    }
                        |> (\m -> { model | playlists = m })
            in
            (case tracks of
                [ t ] ->
                    "Added __" ++ t.title ++ "__"

                l ->
                    "Added __" ++ String.fromInt (List.length l) ++ " tracks__"
            )
                |> (\s -> s ++ " to the __" ++ properPlaylistName ++ "__ playlist")
                |> Notifications.success
                |> showNotificationWithModel newModel
                |> andThen (translateReply SavePlaylists)

        DeactivatePlaylist ->
            Tracks.DeselectPlaylist
                |> TracksMsg
                |> updateWithModel model

        GenerateDirectoryPlaylists ->
            let
                nonDirectoryPlaylists =
                    List.filterNot
                        .autoGenerated
                        model.playlists.collection

                directoryPlaylists =
                    UI.Playlists.Directory.generate
                        model.sources.collection
                        model.tracks.collection.untouched

                playlists =
                    model.playlists
            in
            [ nonDirectoryPlaylists
            , directoryPlaylists
            ]
                |> List.concat
                |> (\c -> { playlists | collection = c })
                |> (\p -> { model | playlists = p })
                |> return

        RemoveFromSelectedPlaylist playlist tracks ->
            let
                updatedPlaylist =
                    Tracks.removeFromPlaylist tracks playlist

                ( tracksModel, playlistsModel ) =
                    ( model.tracks
                    , model.playlists
                    )

                newPlaylistsCollection =
                    List.map
                        (\p ->
                            if p.name == playlist.name then
                                updatedPlaylist

                            else
                                p
                        )
                        model.playlists.collection
            in
            newPlaylistsCollection
                |> (\c -> { playlistsModel | collection = c })
                |> (\p -> { model | playlists = p })
                |> update (TracksMsg <| Tracks.SelectPlaylist updatedPlaylist)
                |> andThen (translateReply SavePlaylists)

        RemovePlaylistFromCollection args ->
            args
                |> Playlists.RemoveFromCollection
                |> PlaylistsMsg
                |> updateWithModel { model | confirmation = Nothing }

        ReplacePlaylistInCollection playlist ->
            let
                playlists =
                    model.playlists
            in
            playlists.collection
                |> List.map (\p -> ifThenElse (p.name == playlist.name) playlist p)
                |> (\c -> { playlists | collection = c })
                |> (\p -> { model | playlists = p })
                |> translateReply SavePlaylists

        RequestAssistanceForPlaylists tracks ->
            model.playlists.collection
                |> List.filterNot .autoGenerated
                |> UI.Playlists.Alfred.create tracks
                |> Alfred.Assign
                |> AlfredMsg
                |> updateWithModel model

        -----------------------------------------
        -- Queue
        -----------------------------------------
        ActiveQueueItemChanged maybeQueueItem ->
            let
                nowPlaying =
                    Maybe.map .identifiedTrack maybeQueueItem

                portCmd =
                    maybeQueueItem
                        |> Maybe.map
                            (.identifiedTrack >> Tuple.second)
                        |> Maybe.map
                            (Queue.makeEngineItem
                                model.currentTime
                                model.sources.collection
                                model.tracks.cached
                                (if model.rememberProgress then
                                    model.progress

                                 else
                                    Dict.empty
                                )
                            )
                        |> Ports.activeQueueItemChanged
            in
            model
                |> update (TracksMsg <| Tracks.SetNowPlaying nowPlaying)
                |> addCommand portCmd

        AddToQueue { inFront, tracks } ->
            (if inFront then
                Queue.InjectFirst

             else
                Queue.InjectLast
            )
                |> (\msg -> msg { showNotification = True } tracks)
                |> QueueMsg
                |> updateWithModel model

        FillQueue ->
            model.tracks.collection.harvested
                |> Queue.Fill model.currentTime
                |> QueueMsg
                |> updateWithModel model

        MoveQueueItemToFirst args ->
            translateReply
                FillQueue
                { model | queue = Queue.moveQueueItemToFirst model.queue args }

        MoveQueueItemToLast args ->
            translateReply
                FillQueue
                { model | queue = Queue.moveQueueItemToLast model.queue args }

        PlayTrack identifiedTrack ->
            identifiedTrack
                |> Queue.InjectFirstAndPlay
                |> QueueMsg
                |> updateWithModel model

        ResetQueue ->
            update (QueueMsg Queue.Reset) model

        RewindQueue ->
            update (QueueMsg Queue.Rewind) model

        ShiftQueue ->
            update (QueueMsg Queue.Shift) model

        ToggleRepeat ->
            update (QueueMsg Queue.ToggleRepeat) model

        ToggleShuffle ->
            update (QueueMsg Queue.ToggleShuffle) model

        -----------------------------------------
        -- Sources & Tracks
        -----------------------------------------
        AddSourceToCollection source ->
            source
                |> Sources.AddToCollection
                |> SourcesMsg
                |> updateWithModel model

        ClearTracksCache ->
            model.tracks.cached
                |> Json.Encode.list Json.Encode.string
                |> Alien.broadcast Alien.RemoveTracksFromCache
                |> Ports.toBrain
                |> returnWithModel (updateTracksModel (\m -> { m | cached = [] }) model)
                |> andThen (update <| TracksMsg Tracks.Harvest)
                |> andThen (translateReply <| SaveEnclosedUserData)
                |> andThen (translateReply <| ShowWarningNotification "Tracks cache was cleared")

        DisableTracksGrouping ->
            Tracks.DisableGrouping
                |> TracksMsg
                |> updateWithModel model

        DownloadTracks zipName tracks ->
            let
                notification =
                    Notifications.stickyWarning "Downloading tracks ..."

                downloading =
                    Just { notificationId = Notifications.id notification }
            in
            [ ( "zipName", Json.Encode.string zipName )
            , ( "trackIds"
              , tracks
                    |> List.map .id
                    |> Json.Encode.list Json.Encode.string
              )
            ]
                |> Json.Encode.object
                |> Alien.broadcast Alien.DownloadTracks
                |> Ports.toBrain
                |> returnWithModel { model | downloading = downloading }
                |> andThen (showNotification notification)

        ExternalSourceAuthorization urlBuilder ->
            model.url
                |> Common.urlOrigin
                |> urlBuilder
                |> Nav.load
                |> returnWithModel model

        ForceTracksRerender ->
            ( model
            , Task.attempt
                (always Bypass)
                (Browser.Dom.setViewportOf UI.Tracks.Scene.List.containerId 0 1)
            )

        GroupTracksBy grouping ->
            grouping
                |> Tracks.GroupBy
                |> TracksMsg
                |> updateWithModel model

        PreloadNextTrack ->
            case List.head model.queue.future of
                Just item ->
                    item
                        |> .identifiedTrack
                        |> Tuple.second
                        |> Queue.makeEngineItem
                            model.currentTime
                            model.sources.collection
                            model.tracks.cached
                            (if model.rememberProgress then
                                model.progress

                             else
                                Dict.empty
                            )
                        |> Ports.preloadAudio
                        |> returnWithModel model

                Nothing ->
                    return model

        ProcessSources [] ->
            return model

        ProcessSources sourcesToProcess ->
            let
                notification =
                    Notifications.stickyWarning "Processing sources ..."

                notificationId =
                    Notifications.id notification

                newNotifications =
                    List.filter
                        (\n -> Notifications.kind n /= Notifications.Error)
                        model.notifications

                sources =
                    model.sources

                isProcessing =
                    sourcesToProcess
                        |> List.sortBy (.data >> Dict.fetch "name" "")
                        |> List.map (\{ id } -> ( id, 0 ))

                newSources =
                    { sources
                        | isProcessing = isProcessing
                        , processingError = Nothing
                        , processingNotificationId = Just notificationId
                    }

                newModel =
                    { model | notifications = newNotifications, sources = newSources }
            in
            [ ( "origin"
              , Json.Encode.string (Common.urlOrigin model.url)
              )
            , ( "sources"
              , Json.Encode.list Sources.encode sourcesToProcess
              )
            ]
                |> Json.Encode.object
                |> Alien.broadcast Alien.ProcessSources
                |> Ports.toBrain
                |> returnWithModel newModel
                |> andThen (showNotification notification)

        RemoveSourceFromCollection args ->
            args
                |> Sources.RemoveFromCollection
                |> SourcesMsg
                |> updateWithModel model

        RemoveTracksFromCache tracks ->
            let
                trackIds =
                    List.map .id tracks
            in
            trackIds
                |> Json.Encode.list Json.Encode.string
                |> Alien.broadcast Alien.RemoveTracksFromCache
                |> Ports.toBrain
                |> returnWithModel
                    (updateTracksModel
                        (\m -> { m | cached = List.without trackIds m.cached })
                        model
                    )
                |> andThen (update <| TracksMsg Tracks.Harvest)
                |> andThen (translateReply SaveEnclosedUserData)

        RemoveTracksWithSourceId sourceId ->
            let
                cmd =
                    sourceId
                        |> Json.Encode.string
                        |> Alien.broadcast Alien.RemoveTracksBySourceId
                        |> Ports.toBrain
            in
            sourceId
                |> Tracks.RemoveBySourceId
                |> TracksMsg
                |> updateWithModel model
                |> addCommand cmd

        ReplaceSourceInCollection source ->
            let
                sources =
                    model.sources
            in
            model.sources.collection
                |> List.map (\s -> ifThenElse (s.id == source.id) source s)
                |> (\c -> { sources | collection = c })
                |> (\s -> { model | sources = s })
                |> return
                |> andThen (translateReply SaveSources)

        ScrollToNowPlaying ->
            update (TracksMsg Tracks.ScrollToNowPlaying) model

        StoreTracksInCache tracks ->
            let
                trackIds =
                    List.map .id tracks

                notification =
                    case tracks of
                        [ t ] ->
                            ("__" ++ t.tags.title ++ "__ will be stored in the cache")
                                |> Notifications.success

                        list ->
                            list
                                |> List.length
                                |> String.fromInt
                                |> (\s -> "__" ++ s ++ " tracks__ will be stored in the cache")
                                |> Notifications.success
            in
            tracks
                |> Json.Encode.list
                    (\track ->
                        Json.Encode.object
                            [ ( "trackId"
                              , Json.Encode.string track.id
                              )
                            , ( "url"
                              , track
                                    |> Queue.makeTrackUrl
                                        model.currentTime
                                        model.sources.collection
                                    |> Json.Encode.string
                              )
                            ]
                    )
                |> Alien.broadcast Alien.StoreTracksInCache
                |> Ports.toBrain
                |> returnWithModel
                    (updateTracksModel
                        (\m -> { m | cachingInProgress = m.cachingInProgress ++ trackIds })
                        model
                    )
                |> andThen (showNotification notification)

        ToggleCachedTracksOnly ->
            update (TracksMsg Tracks.ToggleCachedOnly) model

        ToggleDirectoryPlaylists args ->
            update (SourcesMsg <| Sources.ToggleDirectoryPlaylists args) model

        ToggleHideDuplicates ->
            update (TracksMsg Tracks.ToggleHideDuplicates) model

        ToggleProcessAutomatically ->
            translateReply SaveSettings { model | processAutomatically = not model.processAutomatically }

        -----------------------------------------
        -- User Data
        -----------------------------------------
        ChooseBackdrop filename ->
            filename
                |> Backdrop.Choose
                |> BackdropMsg
                |> updateWithModel model

        Export ->
            { favourites = model.tracks.favourites
            , playlists = List.filterNot .autoGenerated model.playlists.collection
            , progress = model.progress
            , settings = Just (gatherSettings model)
            , sources = model.sources.collection
            , tracks = model.tracks.collection.untouched
            }
                |> encodeHypaethralData
                |> Json.Encode.encode 2
                |> File.Download.string "diffuse.json" "application/json"
                |> returnWithModel model

        InsertDemo ->
            model.currentTime
                |> Demo.tape
                |> LoadHypaethralUserData
                |> updateWithModel model
                |> saveAllHypaethralData

        LoadDefaultBackdrop ->
            Backdrop.Default
                |> BackdropMsg
                |> updateWithModel model

        RequestImport ->
            Import
                |> File.Select.file [ "application/json" ]
                |> returnWithModel model

        SaveEnclosedUserData ->
            model
                |> exportEnclosed
                |> Alien.broadcast Alien.SaveEnclosedUserData
                |> Ports.toBrain
                |> returnWithModel model

        SaveFavourites ->
            model.tracks.favourites
                |> Json.Encode.list Tracks.encodeFavourite
                |> Alien.broadcast Alien.SaveFavourites
                |> Ports.toBrain
                |> returnWithModel model

        SavePlaylists ->
            model.playlists.collection
                |> List.filterNot .autoGenerated
                |> Json.Encode.list Playlists.encode
                |> Alien.broadcast Alien.SavePlaylists
                |> Ports.toBrain
                |> returnWithModel model

        SaveProgress ->
            model.progress
                |> Json.Encode.dict identity Json.Encode.float
                |> Alien.broadcast Alien.SaveProgress
                |> Ports.toBrain
                |> returnWithModel model

        SaveSettings ->
            model
                |> gatherSettings
                |> Settings.encode
                |> Alien.broadcast Alien.SaveSettings
                |> Ports.toBrain
                |> returnWithModel model

        SaveSources ->
            let
                updateEnabledSourceIdsOnTracks =
                    model.sources.collection
                        |> Sources.enabledSourceIds
                        |> Tracks.SetEnabledSourceIds
                        |> TracksMsg
                        |> update

                ( updatedModel, updatedCmd ) =
                    updateEnabledSourceIdsOnTracks model
            in
            updatedModel.sources.collection
                |> Json.Encode.list Sources.encode
                |> Alien.broadcast Alien.SaveSources
                |> Ports.toBrain
                |> returnWithModel updatedModel
                |> addCommand updatedCmd

        SaveTracks ->
            model.tracks.collection.untouched
                |> Json.Encode.list Tracks.encodeTrack
                |> Alien.broadcast Alien.SaveTracks
                |> Ports.toBrain
                |> returnWithModel model


translateReplyWithModel : Model -> Reply -> Return Model Msg
translateReplyWithModel model reply =
    translateReply reply model



-- 📣  ░░  FUNCTIONS


hideOverlay : Model -> Return Model Msg
hideOverlay model =
    ( { model
        | alfred = { instance = Nothing }
        , confirmation = Nothing
        , contextMenu = Nothing
      }
      --
    , Cmd.none
    )


loadSourceForForm : Model -> String -> ( Model, Cmd Msg )
loadSourceForForm model sourceId =
    let
        isLoading =
            model.isLoading

        maybeSource =
            List.find (.id >> (==) sourceId) model.sources.collection
    in
    case ( isLoading, maybeSource ) of
        ( False, Just source ) ->
            let
                ( sources, form ) =
                    ( model.sources
                    , model.sources.form
                    )

                newForm =
                    { form | context = source }

                newSources =
                    { sources | form = newForm }
            in
            return { model | sources = newSources }

        ( False, Nothing ) ->
            return model

        ( True, _ ) ->
            -- Redirect away from edit-source page
            UI.Sources.Page.Index
                |> Page.Sources
                |> ChangeUrlUsingPage
                |> updateWithModel model


resetUrl : Nav.Key -> Url -> Page.Page -> Cmd Msg
resetUrl key url page =
    Nav.replaceUrl key (url.path ++ Page.toString page)


saveAllHypaethralData : Return Model Msg -> Return Model Msg
saveAllHypaethralData return =
    List.foldl
        (\( _, bit ) ->
            case bit of
                Favourites ->
                    andThen (translateReply SaveFavourites)

                Playlists ->
                    andThen (translateReply SavePlaylists)

                Progress ->
                    andThen (translateReply SaveProgress)

                Settings ->
                    andThen (translateReply SaveSettings)

                Sources ->
                    andThen (translateReply SaveSources)

                Tracks ->
                    andThen (translateReply SaveTracks)
        )
        return
        hypaethralBit.list


showNotification : Notification Reply -> Model -> Return Model Msg
showNotification notification model =
    model.notifications
        |> UI.Notifications.show notification
        |> mapModel (\n -> { model | isLoading = False, notifications = n })
        |> mapCommand Reply


showNotificationWithModel : Model -> Notification Reply -> Return Model Msg
showNotificationWithModel model notification =
    showNotification notification model


syncHypaethralData : Model -> Return Model Msg
syncHypaethralData model =
    "Syncing"
        |> Notifications.warning
        |> showNotificationWithModel model
        |> addCommand (Ports.toBrain <| Alien.trigger Alien.SyncHypaethralData)



-- 📰


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fromAlien alien

        -- Audio
        --------
        , Ports.activeQueueItemEnded (QueueMsg << always Queue.Shift)
        , Ports.noteProgress (Audio << Audio.NoteProgress)
        , Ports.setAudioDuration (Audio << Audio.SetDuration)
        , Ports.setAudioHasStalled (Audio << Audio.SetHasStalled)
        , Ports.setAudioIsLoading (Audio << Audio.SetIsLoading)
        , Ports.setAudioIsPlaying (Audio << Audio.SetIsPlaying)
        , Ports.setAudioPosition (Audio << Audio.SetPosition)

        -- Remote
        ---------
        , Ports.requestNext <| always (QueueMsg Queue.Shift)
        , Ports.requestPlayPause <| always (Audio Audio.PlayPause)
        , Ports.requestPrevious <| always (QueueMsg Queue.Rewind)
        , Ports.requestStop <| always (Audio Audio.Stop)

        -- Children
        -----------
        , ifThenElse
            (Maybe.isJust model.alfred.instance)
            (Sub.map AlfredMsg <| Alfred.subscriptions model.alfred)
            Sub.none

        -- Resize
        ---------
        , Browser.Events.onResize
            (\w h ->
                ( w, h )
                    |> ResizedWindow
                    |> Debouncer.provideInput
                    |> Debounce
            )

        --
        , Ports.downloadTracksFinished (\_ -> DownloadTracksFinished)
        , Ports.indicateTouchDevice (\_ -> SetIsTouchDevice True)
        , Ports.preferredColorSchemaChanged PreferredColorSchemaChanged
        , Ports.scrobble Scrobble
        , Ports.setAverageBackgroundColor (Backdrop.BackgroundColor >> BackdropMsg)
        , Ports.setIsOnline SetIsOnline
        , Ports.showErrorNotification (Notifications.error >> ShowNotification)
        , Ports.showStickyErrorNotification (Notifications.stickyError >> ShowNotification)

        --
        , Sub.map KeyboardMsg Keyboard.subscriptions
        , Time.every (60 * 1000) SetCurrentTime
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
            UI.Types.ToggleLoadingScreen Off

        Just Alien.ImportLegacyData ->
            ShowNotification (Notifications.success "Imported data successfully!")

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
                        |> ShowNotification

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
                    ShowNotification (Notifications.error err)

        _ ->
            ShowNotification (Notifications.error err)



-- 🗺


view : Model -> Browser.Document Msg
view model =
    { title = "Diffuse"
    , body = [ body model ]
    }


body : Model -> Html Msg
body model =
    section
        (if Maybe.isJust model.contextMenu || Maybe.isJust model.alfred.instance then
            [ on "tap" (Json.Decode.succeed HideOverlay) ]

         else if Maybe.isJust model.equalizer.activeKnob then
            [ Pointer.onMove (EqualizerMsg << Equalizer.AdjustKnob)
            , Pointer.onUp (EqualizerMsg << Equalizer.DeactivateKnob)
            , Pointer.onCancel (EqualizerMsg << Equalizer.DeactivateKnob)
            ]

         else if model.isDragging then
            [ class C.dragging_something
            , on "mouseup" (Json.Decode.succeed StoppedDragging)
            , on "touchcancel" (Json.Decode.succeed StoppedDragging)
            , on "touchend" (Json.Decode.succeed StoppedDragging)
            ]

         else if Maybe.isJust model.queue.selection then
            [ on "tap" (Json.Decode.succeed RemoveQueueSelection) ]

         else if not (List.isEmpty model.tracks.selectedTrackIndexes) then
            [ on "tap" (Json.Decode.succeed RemoveTrackSelection) ]

         else
            []
        )
        [ -----------------------------------------
          -- Alfred
          -----------------------------------------
          model.alfred
            |> Lazy.lazy Alfred.view
            |> Html.map AlfredMsg

        -----------------------------------------
        -- Backdrop
        -----------------------------------------
        , model.backdrop
            |> Lazy.lazy Backdrop.view
            |> Html.map BackdropMsg

        -----------------------------------------
        -- Context Menu
        -----------------------------------------
        , model.contextMenu
            |> Lazy.lazy UI.ContextMenu.view
            |> Html.map Reply

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        , model.notifications
            |> Lazy.lazy UI.Notifications.view
            |> Html.map Reply

        -----------------------------------------
        -- Overlay
        -----------------------------------------
        , model.contextMenu
            |> Lazy.lazy2 overlay model.alfred.instance

        -----------------------------------------
        -- Content
        -----------------------------------------
        , let
            opts =
                { justifyCenter = False
                , scrolling = not model.isDragging
                }
          in
          case ( model.isLoading, model.authentication ) of
            ( True, _ ) ->
                content { opts | justifyCenter = True } [ loadingAnimation ]

            ( False, Authentication.Authenticated _ ) ->
                content opts (defaultScreen model)

            ( False, _ ) ->
                model.authentication
                    |> Lazy.lazy Authentication.view
                    |> Html.map AuthenticationMsg
                    |> List.singleton
                    |> content opts
        ]


defaultScreen : Model -> List (Html Msg)
defaultScreen model =
    [ Lazy.lazy2
        (Navigation.global
            [ ( Page.Index, "Tracks" )
            , ( Page.Sources UI.Sources.Page.Index, "Sources" )
            , ( Page.Settings UI.Settings.Page.Index, "Settings" )
            ]
        )
        model.alfred.instance
        model.page

    -----------------------------------------
    -- Main
    -----------------------------------------
    , vessel
        [ { amountOfSources = List.length model.sources.collection
          , bgColor = model.backdrop.bgColor
          , darkMode = model.darkMode
          , isOnIndexPage = model.page == Page.Index
          , isTouchDevice = model.isTouchDevice
          , sourceIdsBeingProcessed = List.map Tuple.first model.sources.isProcessing
          , viewport = model.viewport
          }
            |> Tracks.view model.tracks
            |> Html.map TracksMsg

        -- Pages
        --------
        , case model.page of
            Page.Equalizer ->
                model.equalizer
                    |> Lazy.lazy Equalizer.view
                    |> Html.map EqualizerMsg

            Page.Index ->
                nothing

            Page.Playlists subPage ->
                model.backdrop.bgColor
                    |> Lazy.lazy4
                        Playlists.view
                        subPage
                        model.playlists
                        model.tracks.selectedPlaylist
                    |> Html.map PlaylistsMsg

            Page.Queue subPage ->
                model.queue
                    |> Lazy.lazy2 Queue.view subPage
                    |> Html.map QueueMsg

            Page.Settings subPage ->
                { authenticationMethod = Authentication.extractMethod model.authentication
                , chosenBackgroundImage = model.backdrop.chosen
                , hideDuplicateTracks = model.tracks.hideDuplicates
                , lastFm = model.lastFm
                , processAutomatically = model.processAutomatically
                , rememberProgress = model.rememberProgress
                }
                    |> Lazy.lazy2 Settings.view subPage
                    |> Html.map Reply

            Page.Sources subPage ->
                let
                    amountOfTracks =
                        List.length model.tracks.collection.untouched
                in
                model.sources
                    |> Lazy.lazy3 Sources.view { amountOfTracks = amountOfTracks } subPage
                    |> Html.map SourcesMsg
        ]

    -----------------------------------------
    -- Controls
    -----------------------------------------
    , Html.map Reply
        (UI.Console.view
            model.queue.activeItem
            model.queue.repeat
            model.queue.shuffle
            { stalled = model.audioHasStalled
            , loading = model.audioIsLoading
            , playing = model.audioIsPlaying
            }
            ( model.audioPosition
            , model.audioDuration
            )
        )
    ]



-- 🗺  ░░  BITS


content : { justifyCenter : Bool, scrolling : Bool } -> List (Html Msg) -> Html Msg
content { justifyCenter, scrolling } nodes =
    brick
        [ on "focusout" (Json.Decode.succeed Blur)
        , on "focusin" inputFocusDecoder
        , style "height" "calc(var(--vh, 1vh) * 100)"
        ]
        [ C.overflow_x_hidden
        , C.relative
        , C.scrolling_touch
        , C.w_screen
        , C.z_10

        --
        , ifThenElse scrolling C.overflow_y_auto C.overflow_y_hidden
        ]
        [ brick
            [ style "min-width" "280px" ]
            [ C.flex
            , C.flex_col
            , C.items_center
            , C.h_full
            , C.px_4

            --
            , C.md__px_8
            , C.lg__px_16

            --
            , ifThenElse justifyCenter C.justify_center ""
            ]
            nodes
        ]


inputFocusDecoder : Json.Decode.Decoder Msg
inputFocusDecoder =
    Json.Decode.string
        |> Json.Decode.at [ "target", "tagName" ]
        |> Json.Decode.andThen
            (\targetTagName ->
                case targetTagName of
                    "INPUT" ->
                        Json.Decode.succeed FocusedOnInput

                    "TEXTAREA" ->
                        Json.Decode.succeed FocusedOnInput

                    _ ->
                        Json.Decode.fail "NOT_INPUT"
            )


loadingAnimation : Html msg
loadingAnimation =
    Html.map never UI.Svg.Elements.loading


overlay : Maybe (Alfred Reply) -> Maybe (ContextMenu Reply) -> Html Msg
overlay maybeAlfred maybeContextMenu =
    let
        isShown =
            Maybe.isJust maybeAlfred || Maybe.isJust maybeContextMenu
    in
    brick
        [ onClick HideOverlay ]
        [ C.inset_0
        , C.bg_black
        , C.fixed
        , C.transition_1000
        , C.transition_ease
        , C.transition_opacity
        , C.z_30

        --
        , ifThenElse isShown "" C.pointer_events_none
        , ifThenElse isShown C.opacity_40 C.opacity_0
        ]
        []


vessel : List (Html Msg) -> Html Msg
vessel =
    (>>)
        (brick
            [ style "-webkit-mask-image" "-webkit-radial-gradient(white, black)" ]
            [ C.bg_white
            , C.flex
            , C.flex_col
            , C.flex_grow
            , C.overflow_hidden
            , C.relative
            , C.rounded

            -- Dark mode
            ------------
            , C.dark__bg_darkest_hour
            ]
        )
        (bricky
            [ style "min-height" "296px" ]
            [ C.flex
            , C.flex_grow
            , C.rounded
            , C.shadow_lg
            , C.w_full

            --
            , C.lg__max_w_insulation
            , C.lg__min_w_3xl
            ]
        )



-- ⚗️  ░░  HYPAETHRAL DATA


gatherSettings : Model -> Settings.Settings
gatherSettings { backdrop, lastFm, processAutomatically, rememberProgress, tracks } =
    { backgroundImage = backdrop.chosen
    , hideDuplicates = tracks.hideDuplicates
    , lastFm = lastFm.sessionKey
    , processAutomatically = processAutomatically
    , rememberProgress = rememberProgress
    }


importHypaethral : Json.Decode.Value -> Model -> Return3.Return Model Msg Reply
importHypaethral value model =
    case decodeHypaethralData value of
        Ok data ->
            let
                { backdrop, sources } =
                    model

                backdropModel =
                    data.settings
                        |> Maybe.andThen .backgroundImage
                        |> Maybe.withDefault Backdrop.default
                        |> Just
                        |> (\c -> { backdrop | chosen = c })

                sourcesModel =
                    { sources | collection = data.sources }

                ( playlistsModel, playlistsCmd, playlistsReplies ) =
                    Playlists.importHypaethral model.playlists data

                selectedPlaylist =
                    Maybe.andThen
                        (\n -> List.find (.name >> (==) n) playlistsModel.collection)
                        model.playlists.playlistToActivate

                ( tracksModel, tracksCmd, tracksReplies ) =
                    Tracks.importHypaethral model.tracks data selectedPlaylist

                lastFmModel =
                    model.lastFm
            in
            ( { model
                | backdrop = backdropModel
                , playlists = playlistsModel
                , progress = data.progress
                , sources = sourcesModel
                , tracks = tracksModel

                --
                , lastFm = { lastFmModel | sessionKey = Maybe.andThen .lastFm data.settings }
                , processAutomatically = Maybe.unwrap True .processAutomatically data.settings
                , rememberProgress = Maybe.unwrap True .rememberProgress data.settings
              }
              --
            , Cmd.batch
                [ Cmd.map PlaylistsMsg playlistsCmd
                , Cmd.map TracksMsg tracksCmd
                ]
              --
            , playlistsReplies ++ tracksReplies
            )

        Err err ->
            err
                |> Json.Decode.errorToString
                |> ShowErrorNotification
                |> Return3.returnReplyWithModel model



-- ⚗️  ░░  ENCLOSED DATA


exportEnclosed : Model -> Json.Encode.Value
exportEnclosed model =
    let
        equalizerSettings =
            { low = model.equalizer.low
            , mid = model.equalizer.mid
            , high = model.equalizer.high
            , volume = model.equalizer.volume
            }
    in
    encodeEnclosedData
        { cachedTracks = model.tracks.cached
        , equalizerSettings = equalizerSettings
        , grouping = model.tracks.grouping
        , onlyShowCachedTracks = model.tracks.cachedOnly
        , onlyShowFavourites = model.tracks.favouritesOnly
        , repeat = model.queue.repeat
        , searchTerm = model.tracks.searchTerm
        , selectedPlaylist = Maybe.map .name model.tracks.selectedPlaylist
        , shuffle = model.queue.shuffle
        , sortBy = model.tracks.sortBy
        , sortDirection = model.tracks.sortDirection
        }


importEnclosed : Json.Decode.Value -> Model -> Return3.Return Model Msg Reply
importEnclosed value model =
    let
        { equalizer, playlists, queue, tracks } =
            model
    in
    case decodeEnclosedData value of
        Ok data ->
            let
                newEqualizer =
                    { equalizer
                        | low = data.equalizerSettings.low
                        , mid = data.equalizerSettings.mid
                        , high = data.equalizerSettings.high
                        , volume = data.equalizerSettings.volume
                    }

                newPlaylists =
                    { playlists
                        | playlistToActivate = data.selectedPlaylist
                    }

                newQueue =
                    { queue
                        | repeat = data.repeat
                        , shuffle = data.shuffle
                    }

                newTracks =
                    { tracks
                        | cached = data.cachedTracks
                        , cachedOnly = data.onlyShowCachedTracks
                        , favouritesOnly = data.onlyShowFavourites
                        , grouping = data.grouping
                        , searchTerm = data.searchTerm
                        , sortBy = data.sortBy
                        , sortDirection = data.sortDirection
                    }
            in
            ( { model
                | equalizer = newEqualizer
                , playlists = newPlaylists
                , queue = newQueue
                , tracks = newTracks
              }
              --
            , Cmd.batch
                [ Cmd.map EqualizerMsg (Equalizer.adjustAllKnobs newEqualizer)
                , Ports.setRepeat data.repeat
                ]
              --
            , []
            )

        Err err ->
            Return3.return model
