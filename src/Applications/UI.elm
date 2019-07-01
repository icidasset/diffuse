module UI exposing (main)

import Alfred exposing (Alfred)
import Alien
import Authentication
import Authentication.RemoteStorage
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Chunky exposing (..)
import Color
import Color.Ext as Color
import Common exposing (Switch(..))
import Conditional exposing (..)
import ContextMenu exposing (ContextMenu)
import Css exposing (url)
import Css.Global
import Css.Transitions
import Debouncer.Basic as Debouncer
import Dict.Ext as Dict
import File
import File.Download
import File.Select
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Html.Styled as Html exposing (Html, section, toUnstyled)
import Html.Styled.Attributes as Attributes exposing (css, id, style)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy as Lazy
import Json.Decode
import Json.Encode
import Keyboard
import List.Extra as List
import Maybe.Extra as Maybe
import Notifications
import Process
import Return2 exposing (..)
import Return3
import Sources
import Sources.Encoding
import Sources.Services.Dropbox
import Sources.Services.Google
import Tachyons.Classes as T
import Task
import Time
import Tracks
import Tracks.Encoding
import UI.Alfred as Alfred
import UI.Authentication as Authentication
import UI.Authentication.ContextMenu as Authentication
import UI.Backdrop as Backdrop
import UI.Console
import UI.ContextMenu
import UI.Core as Core exposing (Flags, Model, Msg(..))
import UI.DnD as DnD
import UI.Equalizer as Equalizer
import UI.Kit
import UI.Navigation as Navigation
import UI.Notifications
import UI.Page as Page
import UI.Playlists as Playlists
import UI.Playlists.Alfred
import UI.Playlists.ContextMenu as Playlists
import UI.Playlists.Directory
import UI.Ports as Ports
import UI.Queue as Queue
import UI.Queue.Common
import UI.Queue.Core as Queue
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
import UI.Tracks.Core as Tracks
import UI.Tracks.Scene.List
import UI.UserData as UserData
import Url exposing (Url)



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
        maybePage =
            Page.fromUrl url

        page =
            Maybe.withDefault Page.Index maybePage
    in
    { contextMenu = Nothing
    , currentTime = Time.millisToPosix flags.initialTime
    , isDragging = False
    , isLoading = True
    , isOnline = flags.isOnline
    , isTouchDevice = False
    , navKey = key
    , notifications = []
    , page = page
    , pressedKeys = []
    , url = url
    , viewport = flags.viewport

    -- Audio
    --------
    , audioDuration = 0
    , audioHasStalled = False
    , audioIsLoading = False
    , audioIsPlaying = False

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



-- 📣


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bypass ->
            return model

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

        HideAlfred ->
            return { model | alfred = { instance = Nothing } }

        HideContextMenu ->
            return { model | contextMenu = Nothing }

        HideOverlay ->
            return { model | alfred = { instance = Nothing }, contextMenu = Nothing }

        KeyboardMsg subMsg ->
            { model | pressedKeys = Keyboard.update subMsg model.pressedKeys }
                |> (\m ->
                        ifThenElse
                            (List.member Keyboard.Escape m.pressedKeys)
                            (update HideOverlay m)
                            (return m)
                   )

        LoadEnclosedUserData json ->
            model
                |> UserData.importEnclosed json
                |> Return3.wield translateReply

        LoadHypaethralUserData json ->
            model
                |> UserData.importHypaethral json
                |> Return3.wield translateReply

        Reply reply ->
            translateReply reply model

        ResizedWindow ( width, height ) ->
            { height = toFloat height
            , width = toFloat width
            }
                |> (\v -> { model | contextMenu = Nothing, viewport = v })
                |> return

        SetCurrentTime time ->
            let
                sources =
                    model.sources
            in
            ( { model
                | currentTime = time
                , sources = { sources | currentTime = time }
              }
            , Cmd.none
            )

        SetIsOnline bool ->
            andThen
                -- We're caching the user's data in the browser while offline.
                -- If we're back online again, sync all the user's data.
                (case ( bool, model.authentication ) of
                    ( True, Authentication.Authenticated (Authentication.RemoteStorage _) ) ->
                        update SyncUserData

                    _ ->
                        update Bypass
                )
                ( { model | isOnline = bool }
                , Cmd.none
                )

        IndicateTouchDevice ->
            return { model | isTouchDevice = True }

        StoppedDragging ->
            let
                notDragging =
                    { model | isDragging = False }
            in
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
                                |> Tracks.ListDragAndDropMsg
                                |> TracksMsg
                                |> updateWithModel notDragging

                _ ->
                    return notDragging

        Core.ToggleLoadingScreen On ->
            return { model | isLoading = True }

        Core.ToggleLoadingScreen Off ->
            return { model | isLoading = False }

        -----------------------------------------
        -- Audio
        -----------------------------------------
        PlayPause ->
            if Maybe.isNothing model.queue.activeItem then
                update (QueueMsg Queue.Shift) model

            else if model.audioIsPlaying then
                returnWithModel model (Ports.pause ())

            else
                returnWithModel model (Ports.play ())

        Seek percentage ->
            returnWithModel model (Ports.seek percentage)

        SetAudioDuration duration ->
            return { model | audioDuration = duration }

        SetAudioHasStalled hasStalled ->
            return { model | audioHasStalled = hasStalled }

        SetAudioIsLoading isLoading ->
            return { model | audioIsLoading = isLoading }

        SetAudioIsPlaying isPlaying ->
            return { model | audioIsPlaying = isPlaying }

        Stop ->
            returnWithModel model (Ports.pause ())

        Unstall ->
            returnWithModel model (Ports.unstall ())

        -----------------------------------------
        -- Authentication
        -----------------------------------------
        AuthenticationBootFailure err ->
            model
                |> UI.Notifications.show (Notifications.stickyError err)
                |> andThen (translateReply LoadDefaultBackdrop)

        RemoteStorageWebfinger remoteStorage (Ok oauthOrigin) ->
            let
                origin =
                    Common.urlOrigin model.url
            in
            remoteStorage
                |> Authentication.RemoteStorage.oauthAddress
                    { oauthOrigin = oauthOrigin
                    , origin = origin
                    }
                |> Nav.load
                |> returnWithModel model

        RemoteStorageWebfinger _ (Err _) ->
            UI.Notifications.show
                (Notifications.error Authentication.RemoteStorage.webfingerError)
                model

        SyncUserData ->
            model
                |> translateReply SaveFavourites
                |> andThen (translateReply SaveSources)
                |> andThen (translateReply SaveTracksFromCache)
                |> andThen (translateReply <| ShowWarningNotification "Syncing")

        -----------------------------------------
        -- Brain
        -----------------------------------------
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
                        , isProcessing = False
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
                |> addCommand (Ports.activeQueueItemChanged Nothing)
                |> addCommand (Nav.pushUrl model.navKey "")

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
        Export ->
            { favourites = model.tracks.favourites
            , playlists = List.filterNot .autoGenerated model.playlists.collection
            , settings = Just (UserData.gatherSettings model)
            , sources = model.sources.collection
            , tracks = model.tracks.collection.untouched
            }
                |> Authentication.encodeHypaethral
                |> Json.Encode.encode 2
                |> File.Download.string "diffuse.json" "application/json"
                |> returnWithModel model

        Import file ->
            250
                |> Process.sleep
                |> Task.andThen (\_ -> File.toString file)
                |> Task.perform ImportJson
                |> returnWithModel { model | isLoading = True }

        ImportJson json ->
            let
                notification =
                    Notifications.success "Imported data successfully!"
            in
            model
                |> update
                    (json
                        |> Json.Decode.decodeString Json.Decode.value
                        |> Result.withDefault Json.Encode.null
                        |> LoadHypaethralUserData
                    )
                |> andThen (translateReply SaveFavourites)
                |> andThen (translateReply SaveSources)
                |> andThen (translateReply SaveTracks)
                |> andThen (update <| ShowNotification notification)
                |> andThen (update <| ChangeUrlUsingPage Page.Index)

        RequestImport ->
            Import
                |> File.Select.file [ "application/json" ]
                |> returnWithModel model

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        Core.DismissNotification args ->
            UI.Notifications.dismiss model args

        RemoveNotification { id } ->
            model.notifications
                |> List.filter (Notifications.id >> (/=) id)
                |> (\notifications -> { model | notifications = notifications })
                |> return

        ShowNotification notification ->
            UI.Notifications.show notification model

        -----------------------------------------
        -- Page Transitions
        -----------------------------------------
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
                |> (\c -> { form | context = c, step = UI.Sources.Form.How })
                |> (\f -> { sources | form = f })
                |> (\s -> { model | sources = s })
                |> return

        PageChanged (Page.Sources (UI.Sources.Page.Edit sourceId)) ->
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

        PageChanged _ ->
            return model

        -----------------------------------------
        -- Playlists
        -----------------------------------------
        RequestAssistanceForPlaylists tracks ->
            model.playlists.collection
                |> List.filterNot .autoGenerated
                |> UI.Playlists.Alfred.create tracks
                |> Alfred.Assign
                |> AlfredMsg
                |> updateWithModel model

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
            if url.path == "about" then
                returnWithModel model (Nav.load "about")

            else
                returnWithModel model (Nav.pushUrl model.navKey <| Url.toString url)

        LinkClicked (Browser.External href) ->
            returnWithModel model (Nav.load href)

        UrlChanged ({ fragment, query } as urlWithQuery) ->
            let
                url =
                    { urlWithQuery | query = Nothing }
            in
            case ( query, Page.fromUrl url ) of
                ( Nothing, Just page ) ->
                    { model | page = page, url = url }
                        |> return
                        |> andThen (update <| PageChanged page)

                ( Just _, Just page ) ->
                    returnWithModel model (resetUrl model.navKey url page)

                _ ->
                    returnWithModel model (resetUrl model.navKey url Page.Index)


updateWithModel : Model -> Msg -> ( Model, Cmd Msg )
updateWithModel model msg =
    update msg model


resetUrl : Nav.Key -> Url -> Page.Page -> Cmd Msg
resetUrl key url page =
    Nav.replaceUrl key (url.path ++ Page.toString page)



-- 📣  ░░  CHILDREN & REPLIES


translateReply : Reply -> Model -> ( Model, Cmd Msg )
translateReply reply model =
    case reply of
        GoToPage page ->
            page
                |> ChangeUrlUsingPage
                |> updateWithModel model

        StartedDragging ->
            return { model | isDragging = True }

        Reply.ToggleLoadingScreen state ->
            update (Core.ToggleLoadingScreen state) model

        -----------------------------------------
        -- Authentication
        -----------------------------------------
        ExternalAuth Authentication.Blockstack _ ->
            [ ( "origin", Json.Encode.string (Common.urlOrigin model.url) ) ]
                |> Json.Encode.object
                |> Alien.broadcast Alien.RedirectToBlockstackSignIn
                |> Ports.toBrain
                |> returnWithModel model

        ExternalAuth (Authentication.RemoteStorage _) input ->
            input
                |> Authentication.RemoteStorage.parseUserAddress
                |> Maybe.map
                    (Authentication.RemoteStorage.webfingerRequest RemoteStorageWebfinger)
                |> Maybe.unwrap
                    (UI.Notifications.show
                        (Notifications.error Authentication.RemoteStorage.userAddressError)
                        model
                    )
                    (returnWithModel model)

        ExternalAuth _ _ ->
            return model

        -----------------------------------------
        -- Context Menu
        -----------------------------------------
        ShowMoreAuthenticationOptions coordinates ->
            return { model | contextMenu = Just (Authentication.moreOptionsMenu coordinates) }

        ShowPlaylistListMenu coordinates playlist ->
            return { model | contextMenu = Just (Playlists.listMenu playlist coordinates) }

        ShowSourceContextMenu coordinates source ->
            return { model | contextMenu = Just (Sources.sourceMenu source coordinates) }

        ShowTracksContextMenu coordinates tracks ->
            return { model | contextMenu = Just (Tracks.trackMenu tracks model.tracks.selectedPlaylist model.playlists.lastModifiedPlaylist coordinates) }

        ShowTracksViewMenu coordinates maybeGrouping ->
            return { model | contextMenu = Just (Tracks.viewMenu maybeGrouping coordinates) }

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        Reply.DismissNotification options ->
            UI.Notifications.dismiss model options

        ShowErrorNotification string ->
            UI.Notifications.show (Notifications.error string) model

        ShowStickyErrorNotification string ->
            UI.Notifications.show (Notifications.stickyError string) model

        ShowStickyErrorNotificationWithCode string code ->
            UI.Notifications.show (Notifications.errorWithCode string code []) model

        ShowSuccessNotification string ->
            UI.Notifications.show (Notifications.success string) model

        ShowStickySuccessNotification string ->
            UI.Notifications.show (Notifications.stickySuccess string) model

        ShowWarningNotification string ->
            UI.Notifications.show (Notifications.warning string) model

        ShowStickyWarningNotification string ->
            UI.Notifications.show (Notifications.stickyWarning string) model

        -----------------------------------------
        -- Playlists
        -----------------------------------------
        ActivatePlaylist playlist ->
            update (TracksMsg <| Tracks.SelectPlaylist playlist) model

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
                |> ShowSuccessNotification
                |> (\r -> translateReply r newModel)
                |> andThen (translateReply SavePlaylists)

        DeactivatePlaylist ->
            update (TracksMsg <| Tracks.DeselectPlaylist) model

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

        -----------------------------------------
        -- Queue
        -----------------------------------------
        ActiveQueueItemChanged maybeQueueItem ->
            let
                nowPlaying =
                    Maybe.map .identifiedTrack maybeQueueItem

                portCmd =
                    maybeQueueItem
                        |> Maybe.map .identifiedTrack
                        |> Maybe.map
                            (UI.Queue.Common.makeEngineItem
                                model.currentTime
                                model.sources.collection
                            )
                        |> Ports.activeQueueItemChanged
            in
            model
                |> update (TracksMsg <| Tracks.SetNowPlaying nowPlaying)
                |> addCommand portCmd

        FillQueue ->
            model.tracks.collection.harvested
                |> Queue.Fill model.currentTime
                |> QueueMsg
                |> updateWithModel model

        PlayTrack identifiedTrack ->
            identifiedTrack
                |> Queue.InjectFirstAndPlay
                |> QueueMsg
                |> updateWithModel model

        ResetQueue ->
            update (QueueMsg Queue.Reset) model

        ShiftQueue ->
            update (QueueMsg Queue.Shift) model

        -----------------------------------------
        -- Sources & Tracks
        -----------------------------------------
        AddSourceToCollection source ->
            source
                |> Sources.AddToCollection
                |> SourcesMsg
                |> updateWithModel model

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

        PreloadNextTrack ->
            case List.head model.queue.future of
                Just item ->
                    item
                        |> .identifiedTrack
                        |> UI.Queue.Common.makeEngineItem
                            model.currentTime
                            model.sources.collection
                        |> Ports.preloadAudio
                        |> returnWithModel model

                Nothing ->
                    return model

        ProcessSources ->
            -- TODO:
            -- Don't process disabled sources?
            let
                notification =
                    Notifications.stickyWarning "Processing sources …"

                notificationId =
                    Notifications.id notification

                sources =
                    model.sources

                newSources =
                    { sources
                        | processingError = Nothing
                        , processingNotificationId = Just notificationId
                    }
            in
            [ ( "origin"
              , Json.Encode.string (Common.urlOrigin model.url)
              )
            , ( "sources"
              , Json.Encode.list Sources.Encoding.encode model.sources.collection
              )
            , ( "tracks"
              , Json.Encode.list Tracks.Encoding.encodeTrack model.tracks.collection.untouched
              )
            ]
                |> Json.Encode.object
                |> Alien.broadcast Alien.ProcessSources
                |> Ports.toBrain
                |> returnWithModel { model | sources = newSources }
                |> andThen (UI.Notifications.show notification)

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

        -----------------------------------------
        -- User Data
        -----------------------------------------
        InsertDemo ->
            model
                |> update (LoadHypaethralUserData UserData.demo)
                |> andThen (translateReply SaveFavourites)
                |> andThen (translateReply SaveSources)
                |> andThen (translateReply SaveTracks)

        LoadDefaultBackdrop ->
            Backdrop.Default
                |> BackdropMsg
                |> updateWithModel model

        SaveEnclosedUserData ->
            model
                |> UserData.exportEnclosed
                |> Alien.broadcast Alien.SaveEnclosedUserData
                |> Ports.toBrain
                |> returnWithModel model

        SaveFavourites ->
            model
                |> UserData.encodedFavourites
                |> Alien.broadcast Alien.SaveFavourites
                |> Ports.toBrain
                |> returnWithModel model

        SavePlaylists ->
            model
                |> UserData.encodedPlaylists
                |> Alien.broadcast Alien.SavePlaylists
                |> Ports.toBrain
                |> returnWithModel model

        SaveSettings ->
            model
                |> UserData.gatherSettings
                |> Authentication.encodeSettings
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
            updatedModel
                |> UserData.encodedSources
                |> Alien.broadcast Alien.SaveSources
                |> Ports.toBrain
                |> returnWithModel updatedModel
                |> addCommand updatedCmd

        SaveTracks ->
            model
                |> UserData.encodedTracks
                |> Alien.broadcast Alien.SaveTracks
                |> Ports.toBrain
                |> returnWithModel model

        SaveTracksFromCache ->
            Alien.SaveTracks
                |> Alien.trigger
                |> Ports.toBrain
                |> returnWithModel model



-- 📰


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fromAlien alien

        -- Audio
        --------
        , Ports.activeQueueItemEnded (QueueMsg << always Queue.Shift)
        , Ports.setAudioDuration SetAudioDuration
        , Ports.setAudioHasStalled SetAudioHasStalled
        , Ports.setAudioIsLoading SetAudioIsLoading
        , Ports.setAudioIsPlaying SetAudioIsPlaying

        -- Remote
        ---------
        , Ports.requestNext <| always (QueueMsg Queue.Shift)
        , Ports.requestPlayPause <| always PlayPause
        , Ports.requestPrevious <| always (QueueMsg Queue.Rewind)
        , Ports.requestStop <| always Stop

        -- Children
        -----------
        , ifThenElse
            (Maybe.isJust model.alfred.instance)
            (Sub.map AlfredMsg <| Alfred.subscriptions model.alfred)
            Sub.none

        -- ...
        ------
        , Browser.Events.onResize
            (\w h ->
                ( w, h )
                    |> ResizedWindow
                    |> Debouncer.provideInput
                    |> Debounce
            )

        --
        , Ports.showErrorNotification (Notifications.error >> ShowNotification)
        , Ports.setAverageBackgroundColor (Backdrop.BackgroundColor >> BackdropMsg)
        , Ports.setIsOnline SetIsOnline

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
    case Alien.tagFromString event.tag of
        Just Alien.AddTracks ->
            TracksMsg (Tracks.Add event.data)

        Just Alien.AuthMethod ->
            -- My brain told me which auth method we're using,
            -- so we can tell the user in the UI.
            case Authentication.decodeMethod event.data of
                Just method ->
                    AuthenticationMsg (Authentication.SignedIn method)

                Nothing ->
                    Bypass

        Just Alien.FinishedProcessingSources ->
            SourcesMsg Sources.FinishedProcessing

        Just Alien.HideLoadingScreen ->
            Core.ToggleLoadingScreen Off

        Just Alien.LoadEnclosedUserData ->
            LoadEnclosedUserData event.data

        Just Alien.LoadHypaethralUserData ->
            LoadHypaethralUserData event.data

        Just Alien.MissingSecretKey ->
            event.data
                |> Json.Decode.decodeValue (Json.Decode.field "alienMethodTag" Alien.tagDecoder)
                |> Result.map (\tag -> Authentication.MissingSecretKey tag event.data)
                |> Result.map AuthenticationMsg
                |> Result.withDefault Bypass

        Just Alien.NotAuthenticated ->
            -- There's not to do in this case.
            -- (ie. the case when we're not authenticated at the start)
            BackdropMsg Backdrop.Default

        Just Alien.RemoveTracksByPath ->
            TracksMsg (Tracks.RemoveByPaths event.data)

        Just Alien.ReportProcessingError ->
            SourcesMsg (Sources.ReportProcessingError event.data)

        Just Alien.SearchTracks ->
            TracksMsg (Tracks.SetSearchResults event.data)

        Just Alien.UpdateSourceData ->
            SourcesMsg (Sources.UpdateSourceData event.data)

        _ ->
            Bypass


translateAlienError : Alien.Event -> String -> Msg
translateAlienError event err =
    case Alien.tagFromString event.tag of
        Just Alien.AuthAnonymous ->
            AuthenticationBootFailure err

        Just Alien.AuthIpfs ->
            AuthenticationBootFailure err

        Just Alien.AuthRemoteStorage ->
            AuthenticationBootFailure err

        Just Alien.AuthTextile ->
            AuthenticationBootFailure err

        Just _ ->
            err
                |> Notifications.error
                |> ShowNotification

        Nothing ->
            Bypass



-- 🗺


view : Model -> Browser.Document Msg
view model =
    { title = "Diffuse"
    , body = [ toUnstyled (body model) ]
    }


body : Model -> Html Msg
body model =
    section
        (if Maybe.isJust model.contextMenu || Maybe.isJust model.alfred.instance then
            [ onClick HideOverlay ]

         else if Maybe.isJust model.equalizer.activeKnob then
            [ (EqualizerMsg << Equalizer.AdjustKnob)
                |> Pointer.onMove
                |> Attributes.fromUnstyled
            , (EqualizerMsg << Equalizer.DeactivateKnob)
                |> Pointer.onUp
                |> Attributes.fromUnstyled
            , (EqualizerMsg << Equalizer.DeactivateKnob)
                |> Pointer.onCancel
                |> Attributes.fromUnstyled
            ]

         else if model.isDragging then
            [ Attributes.class "dragging-something"
            , Attributes.fromUnstyled (Pointer.onUp <| always StoppedDragging)
            , Attributes.fromUnstyled (Pointer.onCancel <| always StoppedDragging)
            ]

         else if not model.isTouchDevice then
            [ Attributes.fromUnstyled (Touch.onStart <| always IndicateTouchDevice) ]

         else
            []
        )
        [ Css.Global.global globalCss

        -----------------------------------------
        -- Alfred
        -----------------------------------------
        , model.alfred
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

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        , model.notifications
            |> Lazy.lazy UI.Notifications.view

        -----------------------------------------
        -- Overlay
        -----------------------------------------
        , model.contextMenu
            |> Lazy.lazy2 overlay model.alfred.instance

        -----------------------------------------
        -- Content
        -----------------------------------------
        , case ( model.isLoading, model.authentication ) of
            ( True, _ ) ->
                content { justifyCenter = True } [ loadingAnimation ]

            ( False, Authentication.Authenticated _ ) ->
                content { justifyCenter = False } (defaultScreen model)

            ( False, _ ) ->
                model.authentication
                    |> Lazy.lazy Authentication.view
                    |> Html.map AuthenticationMsg
                    |> List.singleton
                    |> content { justifyCenter = False }
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
        [ model
            |> Tracks.view
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
                model.playlists
                    |> Lazy.lazy3 Playlists.view subPage model.tracks.selectedPlaylist
                    |> Html.map PlaylistsMsg

            Page.Queue subPage ->
                model.queue
                    |> Lazy.lazy2 Queue.view subPage
                    |> Html.map QueueMsg

            Page.Settings subPage ->
                Settings.view subPage model

            Page.Sources subPage ->
                model.sources
                    |> Lazy.lazy2 Sources.view subPage
                    |> Html.map SourcesMsg
        ]

    -----------------------------------------
    -- Controls
    -----------------------------------------
    , Lazy.lazy6
        UI.Console.view
        model.queue.activeItem
        model.queue.repeat
        model.queue.shuffle
        model.audioHasStalled
        model.audioIsLoading
        model.audioIsPlaying
    ]



-- 🗺  ░░  BITS


content : { justifyCenter : Bool } -> List (Html msg) -> Html msg
content { justifyCenter } nodes =
    brick
        [ css contentStyles ]
        [ T.overflow_scroll
        , T.relative
        , T.z_1
        ]
        [ brick
            [ css contentInnerStyles ]
            [ T.flex
            , T.flex_column
            , T.items_center
            , T.h_100
            , T.ph3

            --
            , ifThenElse justifyCenter T.justify_center ""
            ]
            nodes
        ]


contentStyles : List Css.Style
contentStyles =
    [ Css.width (Css.vw 100)

    --
    , Css.property "height" "calc(var(--vh, 1vh) * 100)"
    , Css.property "-webkit-overflow-scrolling" "touch"
    ]


contentInnerStyles : List Css.Style
contentInnerStyles =
    [ Css.minWidth (Css.px 280) ]


loadingAnimation : Html msg
loadingAnimation =
    Html.map never (Html.fromUnstyled UI.Svg.Elements.loading)


overlay : Maybe (Alfred Reply) -> Maybe (ContextMenu Msg) -> Html Msg
overlay maybeAlfred maybeContextMenu =
    brick
        [ css overlayStyles ]
        [ T.absolute__fill
        , T.bg_black_40
        , T.fixed
        , T.z_999

        --
        , ifThenElse (Maybe.isJust maybeAlfred || Maybe.isJust maybeContextMenu) T.o_100 T.o_0
        ]
        []


vessel : List (Html Msg) -> Html Msg
vessel =
    (>>)
        (brick
            [ css vesselInnerStyles ]
            [ UI.Kit.borderRadius
            , T.bg_white
            , T.flex
            , T.flex_column
            , T.flex_grow_1
            , T.overflow_hidden
            , T.relative
            ]
        )
        (bricky
            [ css vesselStyles ]
            [ UI.Kit.borderRadius
            , T.flex
            , T.flex_grow_1
            , T.w_100
            ]
        )



-- 🖼  ░░  GLOBAL


globalCss : List Css.Global.Snippet
globalCss =
    [ -----------------------------------------
      -- Body
      -----------------------------------------
      Css.Global.body
        [ Css.color (Color.toElmCssColor UI.Kit.colors.text)
        , Css.fontFamilies UI.Kit.defaultFontFamilies
        , Css.minWidth (Css.px 300)
        , Css.textRendering Css.optimizeLegibility

        -- Font smoothing
        -----------------
        , Css.property "-webkit-font-smoothing" "antialiased"
        , Css.property "-moz-osx-font-smoothing" "grayscale"
        , Css.property "font-smoothing" "antialiased"

        -- Font features
        ----------------
        , Css.property "-moz-font-feature-settings" "\"kern\", \"liga\""
        , Css.property "font-feature-settings" "\"kern\", \"liga\""
        ]

    -----------------------------------------
    -- Placeholders
    -----------------------------------------
    , Css.Global.selector "::-webkit-input-placeholder" placeholderStyles
    , Css.Global.selector "::-moz-placeholder" placeholderStyles
    , Css.Global.selector ":-ms-input-placeholder" placeholderStyles
    , Css.Global.selector ":-moz-placeholder" placeholderStyles
    , Css.Global.selector "::placeholder" placeholderStyles

    -----------------------------------------
    -- Bits & Pieces
    -----------------------------------------
    , Css.Global.selector ".bg-accent" [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colorKit.accent) ]
    , Css.Global.selector ".bg-base-00" [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colorKit.base00) ]
    , Css.Global.selector ".bg-base-01" [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colorKit.base01) ]
    , Css.Global.selector ".bg-base-0D" [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colorKit.base0D) ]
    , Css.Global.selector ".dragging-something" [ Css.cursor Css.grabbing ]
    , Css.Global.selector ".dragging-something *" [ Css.important (Css.cursor Css.grabbing) ]
    , Css.Global.selector ".grab-cursor" [ Css.cursor Css.grab ]
    , Css.Global.selector ".lh-0" [ Css.lineHeight Css.zero ]
    , Css.Global.selector ".pointer-events-none" [ Css.pointerEvents Css.none ]

    --
    , Css.Global.selector ".disable-selection"
        [ Css.property "-webkit-user-select" "none"
        , Css.property "-moz-user-select" "none"
        , Css.property "-ms-user-select" "none"
        , Css.property "user-select" "none"
        ]
    ]


placeholderStyles : List Css.Style
placeholderStyles =
    [ Css.color (Css.rgb 0 0 0)
    , Css.opacity (Css.num 0.2)
    ]



-- 🖼  ░░  OTHER


overlayStyles : List Css.Style
overlayStyles =
    [ Css.pointerEvents Css.none

    --
    , Css.Transitions.transition
        [ Css.Transitions.opacity3 1000 0 Css.Transitions.ease ]
    ]


vesselStyles : List Css.Style
vesselStyles =
    [ Css.boxShadow5 (Css.px 0) (Css.px 2) (Css.px 4) (Css.px 0) (Css.rgba 0 0 0 0.2)
    , Css.maxWidth (Css.px UI.Kit.insulationWidth)
    , Css.minHeight (Css.px 296)
    ]


vesselInnerStyles : List Css.Style
vesselInnerStyles =
    [ Css.property "-webkit-mask-image" "-webkit-radial-gradient(white, black)"
    ]
