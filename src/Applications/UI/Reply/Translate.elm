module UI.Reply.Translate exposing (..)

import Alien
import Browser.Dom
import Browser.Navigation as Nav
import Chunky exposing (..)
import Common exposing (Switch(..))
import Conditional exposing (..)
import File.Download
import File.Select
import Html.Attributes exposing (id)
import Json.Encode
import LastFm
import List.Ext as List
import List.Extra as List
import Monocle.Lens as Lens
import Notifications
import Playlists.Encoding as Playlists
import Queue
import Return exposing (andThen, return)
import Return.Ext as Return
import Settings
import Sources
import Sources.Encoding as Sources
import String.Ext as String
import Task
import Tracks
import Tracks.Encoding as Tracks
import UI.Audio.State as Audio
import UI.Authentication.ContextMenu as Authentication
import UI.Authentication.Types as Authentication
import UI.Backdrop as Backdrop
import UI.Common.State as Common exposing (showNotification, showNotificationWithModel)
import UI.Demo as Demo
import UI.Interface.State as Interface
import UI.Notifications
import UI.Playlists.Alfred
import UI.Playlists.ContextMenu as Playlists
import UI.Playlists.Directory
import UI.Ports as Ports
import UI.Queue.ContextMenu as Queue
import UI.Queue.State as Queue
import UI.Queue.Types as Queue
import UI.Reply as Reply exposing (Reply(..))
import UI.Settings as Settings
import UI.Sources.ContextMenu as Sources
import UI.Sources.State as Sources
import UI.Sources.Types as Sources
import UI.Tracks as Tracks
import UI.Tracks.ContextMenu as Tracks
import UI.Tracks.Scene.List
import UI.Tracks.State as Tracks
import UI.Types as UI exposing (..)
import Url exposing (Protocol(..))
import Url.Ext as Url
import User.Layer exposing (..)
import User.Layer.Methods.RemoteStorage as RemoteStorage



-- 📣  ░░  REPLIES


translate : Reply -> Manager
translate reply model =
    case reply of
        Shunt ->
            Return.singleton model

        --
        CopyToClipboard string ->
            string
                |> Ports.copyToClipboard
                |> return model

        GoToPage page ->
            Common.changeUrlUsingPage page model

        Reply.ToggleLoadingScreen a ->
            Interface.toggleLoadingScreen a model

        -----------------------------------------
        -- Audio
        -----------------------------------------
        Seek percentage ->
            return model (Ports.seek percentage)

        TogglePlayPause ->
            Audio.playPause model

        ToggleRememberProgress ->
            translate
                SaveSettings
                { model | rememberProgress = not model.rememberProgress }

        -----------------------------------------
        -- Authentication
        -----------------------------------------
        ImportLegacyData ->
            Alien.ImportLegacyData
                |> Alien.trigger
                |> Ports.toBrain
                |> return model
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
                        |> Return.performanceF model

        PingTextileForAuth ->
            Authentication.PingTextile
                |> AuthenticationMsg
                |> Return.performanceF model

        ShowUpdateEncryptionKeyScreen authMethod ->
            authMethod
                |> Authentication.ShowUpdateEncryptionKeyScreen
                |> AuthenticationMsg
                |> Return.performanceF model

        SignOut ->
            let
                { sources, tracks } =
                    model
            in
            { model
                | authentication = Authentication.Unauthenticated
                , playlists = []
                , playlistToActivate = Nothing

                --
                , dontPlay = []
                , nowPlaying = Nothing
                , playedPreviously = []
                , playingNext = []
                , selectedQueueItem = Nothing

                --
                , repeat = False
                , shuffle = False

                --
                , processingContext = []
                , sources = []

                --
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
                |> Backdrop.setDefault
                |> Return.command (Ports.toBrain <| Alien.trigger Alien.SignOut)
                |> Return.command (Ports.toBrain <| Alien.trigger Alien.StopProcessing)
                |> Return.command (Ports.activeQueueItemChanged Nothing)
                |> Return.command (Nav.pushUrl model.navKey "#/")

        -----------------------------------------
        -- Context Menu
        -----------------------------------------
        ContextMenuConfirmation conf r ->
            { model | confirmation = Just conf }
                |> Return.singleton
                |> andThen (translate r)

        ReplyViaContextMenu r ->
            case r of
                ContextMenuConfirmation _ _ ->
                    translate r model

                _ ->
                    translate r { model | contextMenu = Nothing }

        ShowMoreAuthenticationOptions coordinates ->
            Return.singleton { model | contextMenu = Just (Authentication.moreOptionsMenu coordinates) }

        Reply.ShowPlaylistListMenu coordinates playlist ->
            Return.singleton { model | contextMenu = Just (Playlists.listMenu playlist model.tracks.collection.identified model.confirmation coordinates) }

        ShowTracksContextMenu coordinates { alt } tracks ->
            let
                menuDependencies =
                    { cached = model.tracks.cached
                    , cachingInProgress = model.tracks.cachingInProgress
                    , currentTime = model.currentTime
                    , selectedPlaylist = model.tracks.selectedPlaylist
                    , lastModifiedPlaylistName = model.lastModifiedPlaylist
                    , showAlternativeMenu = alt
                    , sources = model.sources
                    }
            in
            Return.singleton { model | contextMenu = Just (Tracks.trackMenu menuDependencies tracks coordinates) }

        ShowTracksViewMenu coordinates maybeGrouping ->
            Return.singleton { model | contextMenu = Just (Tracks.viewMenu model.tracks.cachedOnly maybeGrouping coordinates) }

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
                |> return model

        DisconnectLastFm ->
            translate
                SaveSettings
                { model | lastFm = LastFm.disconnect model.lastFm }

        -----------------------------------------
        -- Notifications
        -----------------------------------------
        DismissNotification options ->
            options
                |> UI.Notifications.dismiss model.notifications
                |> Return.map (\n -> { model | notifications = n })
                |> Return.mapCmd Reply

        RemoveNotification { id } ->
            model.notifications
                |> List.filter (Notifications.id >> (/=) id)
                |> (\n -> { model | notifications = n })
                |> Return.singleton

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
        Reply.ActivatePlaylist playlist ->
            playlist
                |> Tracks.SelectPlaylist
                |> TracksMsg
                |> Return.performanceF model

        Reply.AddTracksToPlaylist a ->
            Return.performance (UI.AddTracksToPlaylist a) model

        Reply.DeactivatePlaylist ->
            Tracks.DeselectPlaylist
                |> TracksMsg
                |> Return.performanceF model

        GenerateDirectoryPlaylists ->
            let
                nonDirectoryPlaylists =
                    List.filterNot
                        .autoGenerated
                        model.playlists

                directoryPlaylists =
                    UI.Playlists.Directory.generate
                        model.sources
                        model.tracks.collection.untouched
            in
            [ nonDirectoryPlaylists
            , directoryPlaylists
            ]
                |> List.concat
                |> (\c -> { model | playlists = c })
                |> Return.singleton

        RemoveFromSelectedPlaylist playlist tracks ->
            let
                updatedPlaylist =
                    Tracks.removeFromPlaylist tracks playlist

                tracksModel =
                    model.tracks
            in
            model.playlists
                |> List.map
                    (\p ->
                        if p.name == playlist.name then
                            updatedPlaylist

                        else
                            p
                    )
                |> (\c -> { model | playlists = c })
                |> Return.performance (TracksMsg <| Tracks.SelectPlaylist updatedPlaylist)
                |> andThen (translate SavePlaylists)

        RemovePlaylistFromCollection args ->
            args
                |> DeletePlaylist
                |> Return.performanceF { model | confirmation = Nothing }

        ReplacePlaylistInCollection playlist ->
            model.playlists
                |> List.map (\p -> ifThenElse (p.name == playlist.name) playlist p)
                |> (\c -> { model | playlists = c })
                |> translate SavePlaylists

        RequestAssistanceForPlaylists tracks ->
            model.playlists
                |> List.filterNot .autoGenerated
                |> UI.Playlists.Alfred.create tracks
                |> AssignAlfred
                |> Return.performanceF model

        -----------------------------------------
        -- Queue
        -----------------------------------------
        AddToQueue { inFront, tracks } ->
            (if inFront then
                Queue.InjectFirst

             else
                Queue.InjectLast
            )
                |> (\msg -> msg { showNotification = True } tracks)
                |> QueueMsg
                |> Return.performanceF model

        MoveQueueItemToFirst args ->
            Queue.moveQueueItemToFirst args model

        MoveQueueItemToLast args ->
            Queue.moveQueueItemToLast args model

        PlayTrack identifiedTrack ->
            identifiedTrack
                |> Queue.InjectFirstAndPlay
                |> QueueMsg
                |> Return.performanceF model

        ResetQueue ->
            Return.performance (QueueMsg Queue.Reset) model

        RewindQueue ->
            Return.performance (QueueMsg Queue.Rewind) model

        ShiftQueue ->
            Return.performance (QueueMsg Queue.Shift) model

        ToggleRepeat ->
            Return.performance (QueueMsg Queue.ToggleRepeat) model

        ToggleShuffle ->
            Return.performance (QueueMsg Queue.ToggleShuffle) model

        -----------------------------------------
        -- Sources & Tracks
        -----------------------------------------
        AddSourceToCollection source ->
            source
                |> Sources.AddToCollection
                |> SourcesMsg
                |> Return.performanceF model

        ClearTracksCache ->
            model.tracks.cached
                |> Json.Encode.list Json.Encode.string
                |> Alien.broadcast Alien.RemoveTracksFromCache
                |> Ports.toBrain
                |> return (Lens.modify Tracks.lens (\m -> { m | cached = [] }) model)
                |> andThen (Return.performance <| TracksMsg Tracks.Harvest)
                |> andThen (translate <| Reply.SaveEnclosedUserData)
                |> andThen (translate <| ShowWarningNotification "Tracks cache was cleared")

        DisableTracksGrouping ->
            Tracks.DisableGrouping
                |> TracksMsg
                |> Return.performanceF model

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
                |> return { model | downloading = downloading }
                |> andThen (showNotification notification)

        ExternalSourceAuthorization urlBuilder ->
            model.url
                |> Common.urlOrigin
                |> urlBuilder
                |> Nav.load
                |> return model

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
                |> Return.performanceF model

        ProcessSources sources ->
            Sources.process model

        RemoveSourceFromCollection args ->
            args
                |> Sources.RemoveFromCollection
                |> SourcesMsg
                |> Return.performanceF model

        RemoveTracksFromCache tracks ->
            let
                trackIds =
                    List.map .id tracks
            in
            trackIds
                |> Json.Encode.list Json.Encode.string
                |> Alien.broadcast Alien.RemoveTracksFromCache
                |> Ports.toBrain
                |> return
                    (Lens.modify Tracks.lens
                        (\m -> { m | cached = List.without trackIds m.cached })
                        model
                    )
                |> andThen (Return.performance <| TracksMsg Tracks.Harvest)
                |> andThen (translate Reply.SaveEnclosedUserData)

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
                |> Return.performanceF model
                |> Return.command cmd

        ScrollToNowPlaying ->
            Return.performance (TracksMsg Tracks.ScrollToNowPlaying) model

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
                                        model.sources
                                    |> Json.Encode.string
                              )
                            ]
                    )
                |> Alien.broadcast Alien.StoreTracksInCache
                |> Ports.toBrain
                |> return
                    (Lens.modify Tracks.lens
                        (\m -> { m | cachingInProgress = m.cachingInProgress ++ trackIds })
                        model
                    )
                |> andThen (showNotification notification)

        ToggleCachedTracksOnly ->
            Return.performance (TracksMsg Tracks.ToggleCachedOnly) model

        ToggleDirectoryPlaylists args ->
            Return.performance (SourcesMsg <| Sources.ToggleDirectoryPlaylists args) model

        ToggleHideDuplicates ->
            Return.performance (TracksMsg Tracks.ToggleHideDuplicates) model

        ToggleProcessAutomatically ->
            translate SaveSettings { model | processAutomatically = not model.processAutomatically }

        -----------------------------------------
        -- User Data
        -----------------------------------------
        Reply.ChooseBackdrop filename ->
            filename
                |> UI.ChooseBackdrop
                |> Return.performanceF model

        Export ->
            { favourites = model.tracks.favourites
            , playlists = List.filterNot .autoGenerated model.playlists
            , progress = model.progress
            , settings = Just (gatherSettings model)
            , sources = model.sources
            , tracks = model.tracks.collection.untouched
            }
                |> encodeHypaethralData
                |> Json.Encode.encode 2
                |> File.Download.string "diffuse.json" "application/json"
                |> return model

        InsertDemo ->
            model.currentTime
                |> Demo.tape
                |> LoadHypaethralUserData
                |> Return.performanceF model
                |> saveAllHypaethralData

        LoadDefaultBackdrop ->
            Backdrop.setDefault model

        RequestImport ->
            ImportFile
                |> File.Select.file [ "application/json" ]
                |> return model

        Reply.SaveEnclosedUserData ->
            Return.performance UI.SaveEnclosedUserData model

        SaveFavourites ->
            model.tracks.favourites
                |> Json.Encode.list Tracks.encodeFavourite
                |> Alien.broadcast Alien.SaveFavourites
                |> Ports.toBrain
                |> return model

        SavePlaylists ->
            model.playlists
                |> List.filterNot .autoGenerated
                |> Json.Encode.list Playlists.encode
                |> Alien.broadcast Alien.SavePlaylists
                |> Ports.toBrain
                |> return model

        SaveProgress ->
            model.progress
                |> Json.Encode.dict identity Json.Encode.float
                |> Alien.broadcast Alien.SaveProgress
                |> Ports.toBrain
                |> return model

        SaveSettings ->
            model
                |> gatherSettings
                |> Settings.encode
                |> Alien.broadcast Alien.SaveSettings
                |> Ports.toBrain
                |> return model

        SaveSources ->
            let
                updateEnabledSourceIdsOnTracks =
                    model.sources
                        |> Sources.enabledSourceIds
                        |> Tracks.SetEnabledSourceIds
                        |> TracksMsg
                        |> Return.performance

                ( updatedModel, updatedCmd ) =
                    updateEnabledSourceIdsOnTracks model
            in
            updatedModel.sources
                |> Json.Encode.list Sources.encode
                |> Alien.broadcast Alien.SaveSources
                |> Ports.toBrain
                |> Return.return updatedModel
                |> Return.command updatedCmd

        SaveTracks ->
            model.tracks.collection.untouched
                |> Json.Encode.list Tracks.encodeTrack
                |> Alien.broadcast Alien.SaveTracks
                |> Ports.toBrain
                |> return model


translateWithModel : Model -> Reply -> ( Model, Cmd Msg )
translateWithModel model reply =
    translate reply model


saveAllHypaethralData : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
saveAllHypaethralData return =
    List.foldl
        (\( _, bit ) ->
            case bit of
                Favourites ->
                    andThen (translate SaveFavourites)

                Playlists ->
                    andThen (translate SavePlaylists)

                Progress ->
                    andThen (translate SaveProgress)

                Settings ->
                    andThen (translate SaveSettings)

                Sources ->
                    andThen (translate SaveSources)

                Tracks ->
                    andThen (translate SaveTracks)
        )
        return
        hypaethralBit.list



-- USER


gatherSettings : Model -> Settings.Settings
gatherSettings { chosenBackdrop, lastFm, processAutomatically, rememberProgress, tracks } =
    { backgroundImage = chosenBackdrop
    , hideDuplicates = tracks.hideDuplicates
    , lastFm = lastFm.sessionKey
    , processAutomatically = processAutomatically
    , rememberProgress = rememberProgress
    }
