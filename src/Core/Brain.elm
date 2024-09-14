module Brain exposing (main)

import Alien
import Brain.Common.State as Common
import Brain.Other.State as Other
import Brain.Ports as Ports
import Brain.Sources.Processing.State as Processing
import Brain.Sources.Processing.Types as Processing
import Brain.Tracks.State as Tracks
import Brain.Types exposing (..)
import Brain.User.State as User
import Brain.User.Types as User
import Debouncer.Basic as Debouncer
import Json.Decode as Json
import Return
import Return.Ext as Return
import Sources.Processing as Processing
import Task
import Time
import Time.Ext as Time
import Url
import User.Layer as User



-- 🧠


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- 🌳


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        hypDebouncer =
            2.5
                |> Debouncer.fromSeconds
                |> Debouncer.debounce
                |> Debouncer.accumulateWith Debouncer.allInputs
                |> Debouncer.toDebouncer

        initialUrl =
            flags.initialUrl
                |> Url.fromString
                |> Maybe.withDefault
                    { protocol = Url.Http
                    , host = ""
                    , port_ = Nothing
                    , path = ""
                    , query = Nothing
                    , fragment = Nothing
                    }
    in
    ( -----------------------------------------
      -- Initial model
      -----------------------------------------
      { currentTime = Time.default
      , hypaethralDebouncer = hypDebouncer
      , hypaethralRetrieval = Nothing
      , hypaethralStorage = []
      , hypaethralUserData = User.emptyHypaethralData
      , origin = "ORIGIN_UNKNOWN"
      , processingStatus = Processing.NotProcessing
      , userSyncMethod = Nothing
      }
      -----------------------------------------
      -- Initial command
      -----------------------------------------
    , Cmd.batch
        [ Task.perform SetCurrentTime Time.now
        , User.initialCommand initialUrl
        ]
    )



-- 📣


update : Msg -> Manager
update msg =
    case msg of
        Bypass ->
            Return.singleton

        Cmd a ->
            Return.communicate a

        -----------------------------------------
        -- Tracks
        -----------------------------------------
        DownloadTracks a ->
            Tracks.download a

        GotSearchResults a ->
            Tracks.gotSearchResults a

        MakeArtworkTrackUrls a ->
            Tracks.makeArtworkTrackUrls a

        RemoveTracksBySourceId a ->
            Tracks.removeBySourceId a

        RemoveTracksFromCache a ->
            Tracks.removeFromCache a

        ReplaceTrackTags a ->
            Tracks.replaceTags a

        Search a ->
            Tracks.search a

        StoreTracksInCache a ->
            Tracks.storeInCache a

        SyncTrackTags a ->
            Tracks.syncTrackTags a

        UpdateSearchIndex a ->
            Tracks.updateSearchIndex a

        -----------------------------------------
        -- 🦉 Nested
        -----------------------------------------
        ProcessingMsg a ->
            Processing.update a

        UserMsg a ->
            User.update a

        -----------------------------------------
        -- 📭 Other
        -----------------------------------------
        RefreshedAccessToken a ->
            Other.refreshedAccessToken a

        SetCurrentTime a ->
            Other.setCurrentTime a

        ToCache a ->
            Other.toCache a



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.fromAlien alien
        , Ports.makeArtworkTrackUrls MakeArtworkTrackUrls
        , Ports.refreshedAccessToken RefreshedAccessToken
        , Ports.receiveSearchResults GotSearchResults
        , Ports.receiveTags (ProcessingMsg << Processing.TagsStep)
        , Ports.replaceTags ReplaceTrackTags

        --
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
        Alien.EnclosedData ->
            UserMsg (User.EnclosedDataRetrieved data)

        Alien.SearchTracks ->
            Search data

        -----------------------------------------
        -- From UI
        -----------------------------------------
        Alien.DownloadTracks ->
            DownloadTracks data

        Alien.ProcessSources ->
            ProcessingMsg (Processing.Process data)

        Alien.RefreshedAccessToken ->
            RefreshedAccessToken data

        Alien.RemoveEncryptionKey ->
            UserMsg User.RemoveEncryptionKey

        Alien.RemoveTracksBySourceId ->
            RemoveTracksBySourceId data

        Alien.RemoveTracksFromCache ->
            RemoveTracksFromCache data

        Alien.SaveEnclosedUserData ->
            UserMsg (User.SaveEnclosedData data)

        Alien.SaveFavourites ->
            UserMsg (User.SaveFavourites data)

        Alien.SavePlaylists ->
            UserMsg (User.SavePlaylists data)

        Alien.SaveProgress ->
            UserMsg (User.SaveProgress data)

        Alien.SaveSettings ->
            UserMsg (User.SaveSettings data)

        Alien.SaveSources ->
            UserMsg (User.SaveSources data)

        Alien.SaveTracks ->
            UserMsg (User.SaveTracks data)

        Alien.SetSyncMethod ->
            UserMsg (User.SetSyncMethod data)

        Alien.StopProcessing ->
            ProcessingMsg Processing.StopProcessing

        Alien.StoreTracksInCache ->
            StoreTracksInCache data

        Alien.SyncTrackTags ->
            SyncTrackTags data

        Alien.ToCache ->
            ToCache data

        Alien.UnsetSyncMethod ->
            UserMsg User.UnsetSyncMethod

        Alien.UpdateEncryptionKey ->
            UserMsg (User.UpdateEncryptionKey data)

        _ ->
            Bypass


translateAlienError : Alien.Tag -> Json.Value -> String -> Msg
translateAlienError tag _ err =
    case err of
        "db is undefined" ->
            Common.reportUICmdMsg tag "Can't connect to the browser's IndexedDB. FYI, this is __not supported in Firefox's private mode__."

        _ ->
            Common.reportUICmdMsg tag err
