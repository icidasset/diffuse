module UI.User.State.Import exposing (..)

import File exposing (File)
import Json.Decode
import Json.Encode
import LastFm
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Lens as Lens
import Notifications
import Process
import Return exposing (andThen, return)
import Return.Ext as Return exposing (communicate)
import Return3
import Task
import UI.Audio.State as Audio
import UI.Backdrop as Backdrop
import UI.Common.State as Common exposing (showNotification)
import UI.Equalizer.State as Equalizer
import UI.Page as Page exposing (Page)
import UI.Playlists as Playlists
import UI.Ports as Ports
import UI.Reply exposing (..)
import UI.Reply.Translate as Reply
import UI.Routing.State as Routing
import UI.Sources as Sources
import UI.Tracks as Tracks
import UI.Types as UI exposing (..)
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
        |> Return3.wield Reply.translate
        -- Show notification
        |> andThen
            ("Imported data successfully!"
                |> Notifications.success
                |> showNotification
            )
        -- Clear tracks cache
        |> andThen (Reply.translate ClearTracksCache)
        -- Redirect to index page
        |> andThen (Routing.changeUrlUsingPage Page.Index)
        -----------------------------
        -- Save all the imported data
        -----------------------------
        |> Reply.saveAllHypaethralData


loadEnclosedUserData : Json.Decode.Value -> Manager
loadEnclosedUserData json model =
    model
        |> importEnclosed json
        |> Return3.wield Reply.translate


loadHypaethralUserData : Json.Decode.Value -> Manager
loadHypaethralUserData json model =
    model
        |> importHypaethral json
        |> Return3.wield Reply.translate
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
            (\m ->
                if m.isUpgrading then
                    """
                    Thank you for using Diffuse V1!
                    If you want to import your old data,
                    please go to the [import page](#/settings/import-export).
                    """
                        |> Notifications.stickySuccess
                        |> Common.showNotificationWithModel m

                else
                    Return.singleton m
            )
        |> andThen
            (\m ->
                if m.processAutomatically then
                    m.sources
                        |> Sources.sourcesToProcess
                        |> ProcessSources
                        |> Reply.translateWithModel m

                else
                    Return.singleton m
            )



-- ⚗️  ░░  HYPAETHRAL DATA


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
                , sources = sourcesModel
                , tracks = tracksModel

                --
                , lastFm = { lastFmModel | sessionKey = Maybe.andThen .lastFm data.settings }
                , processAutomatically = Maybe.unwrap True .processAutomatically data.settings
              }
                |> Lens.modify
                    Audio.lens
                    (\a ->
                        { a
                            | progress = data.progress
                            , rememberProgress = Maybe.unwrap True .rememberProgress data.settings
                        }
                    )
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
                [ Equalizer.adjustAllKnobs newEqualizer
                , Ports.setRepeat data.repeat
                ]
              --
            , []
            )

        Err err ->
            Return3.return model
