module UI exposing (main)

import Alfred exposing (Alfred)
import Alien
import Authentication exposing (..)
import Authentication.RemoteStorage
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Chunky exposing (..)
import Classes as C
import Color
import Color.Ext as Color
import Common exposing (Switch(..))
import Conditional exposing (..)
import ContextMenu exposing (ContextMenu)
import Coordinates exposing (Coordinates, Viewport)
import Css exposing (url)
import Css.Global
import Css.Transitions
import Debouncer.Basic as Debouncer exposing (Debouncer)
import Dict.Ext as Dict
import File
import File.Download
import File.Select
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import Html.Styled as Html exposing (Html, section, toUnstyled)
import Html.Styled.Attributes as Attributes exposing (class, css, id, style)
import Html.Styled.Events as Events exposing (on, onClick)
import Html.Styled.Lazy as Lazy
import Json.Decode
import Json.Encode
import Keyboard
import List.Ext as List
import List.Extra as List
import Maybe.Extra as Maybe
import Notifications
import Playlists.Encoding as Playlists
import Process
import Return2 exposing (..)
import Return3
import Sources
import Sources.Encoding as Sources
import Sources.Services.Dropbox
import Sources.Services.Google
import Tachyons.Classes as T
import Task
import Time
import Tracks
import Tracks.Encoding as Tracks
import UI.Alfred as Alfred
import UI.Authentication as Authentication
import UI.Authentication.ContextMenu as Authentication
import UI.Backdrop as Backdrop
import UI.Console
import UI.ContextMenu
import UI.Core as Core exposing (..)
import UI.Demo as Demo
import UI.DnD as DnD
import UI.Equalizer as Equalizer
import UI.Kit
import UI.Navigation as Navigation
import UI.Notifications
import UI.Page as Page exposing (Page)
import UI.Playlists as Playlists
import UI.Playlists.Alfred
import UI.Playlists.ContextMenu as Playlists
import UI.Playlists.Directory
import UI.Ports as Ports
import UI.Queue as Queue
import UI.Queue.Common
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
import Url exposing (Url)



-- ⛩


type alias Flags =
    { initialTime : Int
    , isOnline : Bool
    , viewport : Viewport
    }


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


type alias Model =
    { contextMenu : Maybe (ContextMenu Msg)
    , currentTime : Time.Posix
    , debounce : Debouncer Msg Msg
    , isDragging : Bool
    , isLoading : Bool
    , isOnline : Bool
    , navKey : Nav.Key
    , notifications : UI.Notifications.Model
    , page : Page
    , pressedKeys : List Keyboard.Key
    , url : Url
    , viewport : Viewport

    -----------------------------------------
    -- Audio
    -----------------------------------------
    , audioDuration : Float
    , audioHasStalled : Bool
    , audioIsLoading : Bool
    , audioIsPlaying : Bool

    -----------------------------------------
    -- Children
    -----------------------------------------
    , alfred : Alfred.Model
    , authentication : Authentication.Model
    , backdrop : Backdrop.Model
    , equalizer : Equalizer.Model
    , queue : Queue.Model
    , playlists : Playlists.Model
    , sources : Sources.Model
    , tracks : Tracks.Model
    }


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
    { contextMenu = Nothing
    , currentTime = Time.millisToPosix flags.initialTime
    , isDragging = False
    , isLoading = True
    , isOnline = flags.isOnline
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

        CopyToClipboard string ->
            returnWithModel model (Ports.copyToClipboard string)

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
                |> importEnclosed json
                |> Return3.wield translateReply

        LoadHypaethralUserData json ->
            model
                |> importHypaethral json
                |> Return3.wield translateReply

        MsgViaContextMenu m ->
            update m { model | contextMenu = Nothing }

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
                                |> UI.Tracks.Scene.List.DragAndDropMsg
                                |> Tracks.ListSceneMsg
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
            Notifications.stickyError err
                |> ShowNotification
                |> updateWithModel model
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
            Authentication.RemoteStorage.webfingerError
                |> ShowErrorNotification
                |> translateReplyWithModel model

        SyncUserData ->
            model
                |> translateReply SaveFavourites
                |> andThen (translateReply SaveSources)
                |> andThen (translateReply SaveTracksFromBrain)
                |> andThen (translateReply <| ShowWarningNotification "Syncing")

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

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        ShowNotification notification ->
            model.notifications
                |> UI.Notifications.show notification
                |> mapModel (\n -> { model | notifications = n })
                |> mapCommand Reply

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
                |> (\c -> { form | context = c, step = UI.Sources.Form.By })
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
        -- Tracks Cache
        -----------------------------------------
        FailedToStoreTracksInCache trackIds ->
            model
                |> updateTracksModel
                    (\m -> { m | cachingInProgress = List.without trackIds m.cachingInProgress })
                |> return
                |> andThen
                    (translateReply <| ShowErrorNotification "Failed to store track in cache")

        FinishedStoringTracksInCache trackIds ->
            -- TODO: When a context menu of a track is open,
            --       it should be "rerendered" in case
            --       the track is no longer being downloaded.
            model
                |> updateTracksModel
                    (\m ->
                        { m
                            | cached = m.cached ++ trackIds
                            , cachingInProgress = List.without trackIds m.cachingInProgress
                        }
                    )
                |> update (TracksMsg Tracks.Harvest)
                |> andThen (translateReply SaveEnclosedUserData)

        RemoveFromTracksCache tracks ->
            let
                trackIds =
                    List.map .id tracks
            in
            tracks
                |> Json.Encode.list (.id >> Json.Encode.string)
                |> Alien.broadcast Alien.RemoveTracksFromCache
                |> Ports.toBrain
                |> returnWithModel
                    (updateTracksModel
                        (\m -> { m | cached = List.without trackIds m.cached })
                        model
                    )
                |> andThen (update <| TracksMsg Tracks.Harvest)
                |> andThen (translateReply SaveEnclosedUserData)

        StoreInTracksCache tracks ->
            let
                trackIds =
                    List.map .id tracks
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
                                    |> UI.Queue.Common.makeTrackUrl
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


updateTracksModel : (Tracks.Model -> Tracks.Model) -> Model -> Model
updateTracksModel fn model =
    { model | tracks = fn model.tracks }


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
        -- Audio
        -----------------------------------------
        Seek percentage ->
            returnWithModel model (Ports.seek percentage)

        TogglePlayPause ->
            update PlayPause model

        -----------------------------------------
        -- Authentication
        -----------------------------------------
        ExternalAuth Authentication.Blockstack _ ->
            Alien.RedirectToBlockstackSignIn
                |> Alien.trigger
                |> Ports.toBrain
                |> returnWithModel model

        ExternalAuth (Authentication.RemoteStorage _) input ->
            input
                |> Authentication.RemoteStorage.parseUserAddress
                |> Maybe.map
                    (Authentication.RemoteStorage.webfingerRequest RemoteStorageWebfinger)
                |> Maybe.unwrap
                    (translateReply
                        (ShowErrorNotification Authentication.RemoteStorage.userAddressError)
                        model
                    )
                    (returnWithModel model)

        ExternalAuth _ _ ->
            return model

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
                |> addCommand (Ports.activeQueueItemChanged Nothing)
                |> addCommand (Nav.pushUrl model.navKey "#/")

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
            return { model | contextMenu = Just (Tracks.trackMenu tracks model.tracks.cachingInProgress model.tracks.cached model.tracks.selectedPlaylist model.playlists.lastModifiedPlaylist coordinates) }

        ShowTracksViewMenu coordinates maybeGrouping ->
            return { model | contextMenu = Just (Tracks.viewMenu model.tracks.cachedOnly maybeGrouping coordinates) }

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
            Notifications.error string
                |> ShowNotification
                |> updateWithModel model

        ShowStickyErrorNotification string ->
            Notifications.stickyError string
                |> ShowNotification
                |> updateWithModel model

        ShowStickyErrorNotificationWithCode string code ->
            Notifications.errorWithCode string code []
                |> ShowNotification
                |> updateWithModel model

        ShowSuccessNotification string ->
            Notifications.success string
                |> ShowNotification
                |> updateWithModel model

        ShowStickySuccessNotification string ->
            Notifications.stickySuccess string
                |> ShowNotification
                |> updateWithModel model

        ShowWarningNotification string ->
            Notifications.warning string
                |> ShowNotification
                |> updateWithModel model

        ShowStickyWarningNotification string ->
            Notifications.stickyWarning string
                |> ShowNotification
                |> updateWithModel model

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
                        |> Maybe.map
                            (.identifiedTrack >> Tuple.second)
                        |> Maybe.map
                            (UI.Queue.Common.makeEngineItem
                                model.currentTime
                                model.sources.collection
                                model.tracks.cached
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
                        |> Tuple.second
                        |> UI.Queue.Common.makeEngineItem
                            model.currentTime
                            model.sources.collection
                            model.tracks.cached
                        |> Ports.preloadAudio
                        |> returnWithModel model

                Nothing ->
                    return model

        ProcessSources ->
            let
                notification =
                    Notifications.stickyWarning "Processing sources …"

                notificationId =
                    Notifications.id notification

                sources =
                    model.sources

                sourcesToProcess =
                    Sources.sourcesToProcess model.sources

                newSources =
                    { sources
                        | isProcessing = List.map .id sourcesToProcess
                        , processingError = Nothing
                        , processingNotificationId = Just notificationId
                    }
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
                |> returnWithModel { model | sources = newSources }
                |> andThen (update <| ShowNotification notification)

        RemoveTracksFromCache tracks ->
            update
                (RemoveFromTracksCache tracks)
                model

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

        ToggleHideDuplicates ->
            update (TracksMsg Tracks.ToggleHideDuplicates) model

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
            , settings = Just (gatherSettings model)
            , sources = model.sources.collection
            , tracks = model.tracks.collection.untouched
            }
                |> Authentication.encodeHypaethral
                |> Json.Encode.encode 2
                |> File.Download.string "diffuse.json" "application/json"
                |> returnWithModel model

        InsertDemo ->
            model
                |> update (LoadHypaethralUserData Demo.tape)
                |> andThen (translateReply SaveFavourites)
                |> andThen (translateReply SaveSources)
                |> andThen (translateReply SaveTracks)

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

        SaveSettings ->
            model
                |> gatherSettings
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

        SaveTracksFromBrain ->
            Alien.SaveTracks
                |> Alien.trigger
                |> Ports.toBrain
                |> returnWithModel model


translateReplyWithModel : Model -> Reply -> ( Model, Cmd Msg )
translateReplyWithModel model reply =
    translateReply reply model



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

        Just Alien.FinishedProcessingSource ->
            event.data
                |> Json.Decode.decodeValue Json.Decode.string
                |> Result.map (Sources.FinishedProcessingSource >> SourcesMsg)
                |> Result.withDefault Bypass

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

        Just Alien.StoreTracksInCache ->
            case Json.Decode.decodeValue (Json.Decode.list Json.Decode.string) event.data of
                Ok list ->
                    FinishedStoringTracksInCache list

                Err jsonErr ->
                    jsonErr
                        |> Json.Decode.errorToString
                        |> Notifications.error
                        |> ShowNotification

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

        Just Alien.StoreTracksInCache ->
            case Json.Decode.decodeValue (Json.Decode.list Json.Decode.string) event.data of
                Ok trackIds ->
                    FailedToStoreTracksInCache trackIds

                Err _ ->
                    err
                        |> Notifications.error
                        |> ShowNotification

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
            [ on "tap" (Json.Decode.succeed HideOverlay) ]

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
            [ class ("dragging-something " ++ C.disable_selection)
            , on "mouseup" (Json.Decode.succeed StoppedDragging)
            , on "touchcancel" (Json.Decode.succeed StoppedDragging)
            , on "touchend" (Json.Decode.succeed StoppedDragging)
            ]

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
          , isOnIndexPage = model.page == Page.Index
          , sourceIdsBeingProcessed = model.sources.isProcessing
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
                model.playlists
                    |> Lazy.lazy3 Playlists.view subPage model.tracks.selectedPlaylist
                    |> Html.map PlaylistsMsg

            Page.Queue subPage ->
                model.queue
                    |> Lazy.lazy2 Queue.view subPage
                    |> Html.map QueueMsg

            Page.Settings subPage ->
                { authenticationMethod = Authentication.extractMethod model.authentication
                , chosenBackgroundImage = model.backdrop.chosen
                , hideDuplicateTracks = model.tracks.hideDuplicates
                }
                    |> Lazy.lazy2 Settings.view subPage
                    |> Html.map Reply

            Page.Sources subPage ->
                model.sources
                    |> Lazy.lazy2 Sources.view subPage
                    |> Html.map SourcesMsg
        ]

    -----------------------------------------
    -- Controls
    -----------------------------------------
    , Html.map Reply
        (Lazy.lazy6
            UI.Console.view
            model.queue.activeItem
            model.queue.repeat
            model.queue.shuffle
            model.audioHasStalled
            model.audioIsLoading
            model.audioIsPlaying
        )
    ]



-- 🗺  ░░  BITS


content : { justifyCenter : Bool, scrolling : Bool } -> List (Html msg) -> Html msg
content { justifyCenter, scrolling } nodes =
    brick
        [ css contentStyles ]
        [ T.overflow_x_hidden
        , T.relative
        , T.z_1

        --
        , ifThenElse scrolling T.overflow_y_auto T.overflow_y_hidden
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
        [ Css.property "-webkit-touch-callout" "none"
        , Css.property "-webkit-user-select" "none"
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



-- ⚗️  ░░  HYPAETHRAL DATA


gatherSettings : Model -> Settings
gatherSettings { backdrop, tracks } =
    { backgroundImage = backdrop.chosen
    , hideDuplicates = tracks.hideDuplicates
    }


importHypaethral : Json.Decode.Value -> Model -> Return3.Return Model Msg Reply
importHypaethral value model =
    case decodeHypaethral value of
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
            in
            ( { model
                | backdrop = backdropModel
                , playlists = playlistsModel
                , sources = sourcesModel
                , tracks = tracksModel
              }
            , Cmd.batch
                [ Cmd.map PlaylistsMsg playlistsCmd
                , Cmd.map TracksMsg tracksCmd
                ]
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
    encodeEnclosed
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
    case decodeEnclosed value of
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
            , Cmd.batch
                [ Cmd.map EqualizerMsg (Equalizer.adjustAllKnobs newEqualizer)
                , Ports.setRepeat data.repeat
                ]
            , []
            )

        Err err ->
            Return3.return model
