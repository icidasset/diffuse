module Brain exposing (main)

import Alien
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
import Json.Encode
import Maybe.Extra as Maybe
import Return
import Return.Ext as Return
import Sources.Processing as Processing
import Sources.Processing.Encoding as Processing
import Task
import Time
import Time.Ext as Time
import Tracks.Encoding as Tracks
import Url
import User.Layer as User exposing (HypaethralBit(..))



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
      { authMethod = Nothing
      , currentTime = Time.default
      , hypaethralDebouncer = hypDebouncer
      , hypaethralRetrieval = Nothing
      , hypaethralStorage = []
      , hypaethralUserData = User.emptyHypaethralData
      , legacyMode = False
      , migratingData = False
      , origin = "ORIGIN_UNKNOWN"
      , performingSignIn = False
      , processingStatus = Processing.NotProcessing
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
        SetCurrentTime a ->
            Other.setCurrentTime a

        ToCache a ->
            Other.toCache a



-- 📰


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.fromAlien alien
        , Ports.makeArtworkTrackUrls MakeArtworkTrackUrls
        , Ports.receiveSearchResults GotSearchResults
        , Ports.receiveTags (ProcessingMsg << Processing.TagsStep)
        , Ports.replaceTags ReplaceTrackTags
        , Ports.savedHypaethralBit (\_ -> UserMsg User.SaveNextHypaethralBit)

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
        Alien.AuthAnonymous ->
            UserMsg (User.HypaethralDataRetrieved data)

        Alien.AuthDropbox ->
            UserMsg (User.HypaethralDataRetrieved data)

        Alien.AuthEnclosedData ->
            UserMsg (User.EnclosedDataRetrieved data)

        Alien.AuthFission ->
            UserMsg (User.HypaethralDataRetrieved data)

        Alien.AuthIpfs ->
            UserMsg (User.HypaethralDataRetrieved data)

        Alien.AuthMethod ->
            UserMsg (User.MethodRetrieved data)

        Alien.AuthRemoteStorage ->
            UserMsg (User.HypaethralDataRetrieved data)

        Alien.FabricateSecretKey ->
            UserMsg User.SecretKeyFabricated

        Alien.SearchTracks ->
            Search data

        -----------------------------------------
        -- From UI
        -----------------------------------------
        Alien.DownloadTracks ->
            DownloadTracks data

        Alien.ImportLegacyData ->
            UserMsg User.RetrieveLegacyHypaethralData

        Alien.ProcessSources ->
            ProcessingMsg (Processing.Process data)

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

        Alien.SignIn ->
            UserMsg (User.SignIn data)

        Alien.SignOut ->
            UserMsg User.SignOut

        Alien.StopProcessing ->
            ProcessingMsg Processing.StopProcessing

        Alien.StoreTracksInCache ->
            StoreTracksInCache data

        Alien.SyncTrackTags ->
            SyncTrackTags data

        Alien.ToCache ->
            ToCache data

        Alien.UpdateEncryptionKey ->
            UserMsg (User.UpdateEncryptionKey data)

        _ ->
            Bypass


translateAlienError : Alien.Tag -> Json.Value -> String -> Msg
translateAlienError tag _ err =
    case tag of
        Alien.AuthAnonymous ->
            reportAuthError Alien.AuthAnonymous err "I found some encrypted data, but I couldn't decrypt it. Maybe you used the wrong passphrase?"

        Alien.AuthDropbox ->
            reportAuthError Alien.AuthDropbox err "I found some encrypted data, but I couldn't decrypt it. Maybe you used the wrong passphrase?"

        Alien.AuthIpfs ->
            reportAuthError Alien.AuthIpfs err "Something went wrong regarding the IPFS storage. Maybe you used the wrong passphrase, or your IPFS node is offline?"

        Alien.AuthRemoteStorage ->
            reportAuthError Alien.AuthRemoteStorage err "I found some encrypted data, but I couldn't decrypt it. Maybe you used the wrong passphrase?"

        _ ->
            case err of
                "db is undefined" ->
                    report tag "Can't connect to the browser's IndexedDB. FYI, this is __not supported in Firefox's private mode__."

                _ ->
                    report tag err


reportAuthError : Alien.Tag -> String -> String -> Msg
reportAuthError tag originalError fallbackError =
    case originalError of
        "MISSING_SECRET_KEY" ->
            [ ( "alienMethodTag", Alien.tagToJson tag )
            , ( "fallbackError", Json.Encode.string fallbackError )
            ]
                |> Json.Encode.object
                |> Alien.broadcast Alien.MissingSecretKey
                |> Ports.toUI
                |> Cmd

        _ ->
            report tag fallbackError


report : Alien.Tag -> String -> Msg
report tag err =
    err
        |> Alien.report tag
        |> Ports.toUI
        |> Cmd
