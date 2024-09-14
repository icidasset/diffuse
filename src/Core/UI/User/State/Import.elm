module UI.User.State.Import exposing (..)

import File exposing (File)
import File.Select
import Json.Decode
import Json.Encode
import LastFm
import List.Extra as List
import Maybe.Extra as Maybe
import Notifications
import Process
import Return exposing (andThen, return)
import Return.Ext exposing (communicate)
import Task
import UI.Backdrop as Backdrop
import UI.Common.State as Common exposing (showNotification)
import UI.Demo as Demo
import UI.Equalizer.State as Equalizer
import UI.Page as Page
import UI.Playlists.Directory
import UI.Sources.State as Sources
import UI.Tracks.State as Tracks
import UI.Types as UI exposing (..)
import UI.User.State.Export as User
import Url.Ext as Url
import User.Layer exposing (..)



-- 🔱


importFile : File -> Manager
importFile file model =
    250
        |> Process.sleep
        |> Task.andThen (\_ -> File.toString file)
        |> Task.perform UI.ImportJson
        |> return { model | isLoading = True }


importJson : String -> Manager
importJson json model =
    json
        -- Load data on main thread (this app)
        |> Json.Decode.decodeString Json.Decode.value
        |> Result.withDefault Json.Encode.null
        |> (\j -> importHypaethral j model)
        -- Show notification
        |> andThen
            ("Imported data successfully!"
                |> Notifications.success
                |> showNotification
            )
        -- Clear tracks cache
        |> andThen Tracks.clearCache
        -- Redirect to index page
        |> andThen Common.forceTracksRerender
        |> andThen (Common.changeUrlUsingPage Page.Index)
        -----------------------------
        -- Save all the imported data
        -----------------------------
        |> saveAllHypaethralData


insertDemo : Manager
insertDemo model =
    model
        |> loadHypaethralUserData (Demo.tape model.currentTime)
        |> saveAllHypaethralData


loadEnclosedUserData : Json.Decode.Value -> Manager
loadEnclosedUserData =
    importEnclosed


loadHypaethralUserData : Json.Decode.Value -> Manager
loadHypaethralUserData json model =
    model
        |> importHypaethral json
        |> andThen
            (\m ->
                case Url.action m.url of
                    [ "authenticate", "lastfm" ] ->
                        { authenticating = True
                        , sessionKey = Nothing
                        }
                            |> (\n -> { m | lastFm = n })
                            |> communicate (LastFm.authenticationCommand GotLastFmSession m.url)

                    _ ->
                        Return.singleton m
            )
        |> andThen
            Sources.addSourcesFromUrl


requestImport : Manager
requestImport model =
    ImportFile
        |> File.Select.file [ "application/json" ]
        |> return model



-- ⚗️  ░░  HYPAETHRAL DATA


importHypaethral : Json.Decode.Value -> Manager
importHypaethral value model =
    case decodeHypaethralData value of
        Ok data ->
            let
                chosenBackdrop =
                    data.settings
                        |> Maybe.andThen .backgroundImage
                        |> Maybe.withDefault Backdrop.default
                        |> Just

                newPlaylistsCollection =
                    List.append
                        data.playlists
                        (UI.Playlists.Directory.generate data.sources data.tracks)

                selectedPlaylist =
                    Maybe.andThen
                        (\n -> List.find (.name >> (==) n) newPlaylistsCollection)
                        model.playlistToActivate

                lastFmModel =
                    model.lastFm
            in
            Tracks.importHypaethral
                data
                selectedPlaylist
                { model
                    | chosenBackdrop = chosenBackdrop
                    , coverSelectionReducesPool = Maybe.unwrap True .coverSelectionReducesPool data.settings
                    , hideDuplicates = Maybe.unwrap False .hideDuplicates data.settings
                    , lastFm = { lastFmModel | sessionKey = Maybe.andThen .lastFm data.settings }
                    , playlists = newPlaylistsCollection
                    , playlistToActivate = Nothing
                    , processAutomatically = Maybe.unwrap True .processAutomatically data.settings
                    , progress = data.progress
                    , rememberProgress = Maybe.unwrap True .rememberProgress data.settings
                    , sources = data.sources
                }

        Err err ->
            err
                |> Json.Decode.errorToString
                |> Notifications.error
                |> Common.showNotificationWithModel model


saveAllHypaethralData : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
saveAllHypaethralData return =
    List.foldl
        (\( _, bit ) ->
            case bit of
                Favourites ->
                    andThen User.saveFavourites

                ModifiedAt ->
                    andThen User.saveModifiedAt

                Playlists ->
                    andThen User.savePlaylists

                Progress ->
                    andThen User.saveProgress

                Settings ->
                    andThen User.saveSettings

                Sources ->
                    andThen User.saveSources

                Tracks ->
                    andThen User.saveTracks
        )
        return
        hypaethralBit.list



-- ⚗️  ░░  ENCLOSED DATA


importEnclosed : Json.Decode.Value -> Manager
importEnclosed value model =
    let
        equalizerSettings =
            model.eqSettings
    in
    case decodeEnclosedData value of
        Ok data ->
            let
                newEqualizerSettings =
                    { equalizerSettings
                        | low = data.equalizerSettings.low
                        , mid = data.equalizerSettings.mid
                        , high = data.equalizerSettings.high
                        , volume = data.equalizerSettings.volume
                    }
            in
            ( { model
                | eqSettings = newEqualizerSettings
                , playlistToActivate = data.selectedPlaylist
                , repeat = data.repeat
                , shuffle = data.shuffle

                -- Tracks
                , cachedTracks = data.cachedTracks
                , cachedTracksOnly = data.onlyShowCachedTracks
                , favouritesOnly = data.onlyShowFavourites
                , grouping = data.grouping
                , scene = data.scene
                , searchTerm = data.searchTerm
                , sortBy = data.sortBy
                , sortDirection = data.sortDirection
                , theme = data.theme
              }
              --
            , Equalizer.adjustAllKnobs newEqualizerSettings
            )

        Err err ->
            ("Failed to decode enclosed data: " ++ Json.Decode.errorToString err)
                |> Notifications.error
                |> Common.showNotificationWithModel model
