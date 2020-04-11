module UI.Alien exposing (..)

import Alien
import Common exposing (Switch(..))
import Json.Decode
import Notifications
import UI.Authentication.Types as Authentication
import UI.Sources.Types as Sources
import UI.Tracks.Types as Tracks
import UI.Types exposing (..)
import User.Layer exposing (..)



-- 📣


translate : Alien.Event -> Msg
translate event =
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
                |> Result.map (\id -> { sourceId = id })
                |> Result.map (Sources.FinishedProcessingSource >> SourcesMsg)
                |> Result.withDefault Bypass

        Just Alien.FinishedProcessingSources ->
            SourcesMsg Sources.FinishedProcessing

        Just Alien.HideLoadingScreen ->
            ToggleLoadingScreen Off

        Just Alien.ImportLegacyData ->
            ShowNotification (Notifications.success "Imported data successfully!")

        Just Alien.LoadEnclosedUserData ->
            LoadEnclosedUserData event.data

        Just Alien.LoadHypaethralUserData ->
            LoadHypaethralUserData event.data

        Just Alien.MissingSecretKey ->
            AuthenticationMsg (Authentication.MissingSecretKey event.data)

        Just Alien.NotAuthenticated ->
            AuthenticationMsg Authentication.NotAuthenticated

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
                    TracksMsg (Tracks.FinishedStoringInCache list)

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
            AuthenticationMsg (Authentication.BootFailure err)

        Just Alien.AuthBlockstack ->
            AuthenticationMsg (Authentication.BootFailure err)

        Just Alien.AuthDropbox ->
            AuthenticationMsg (Authentication.BootFailure err)

        Just Alien.AuthIpfs ->
            AuthenticationMsg (Authentication.BootFailure err)

        Just Alien.AuthRemoteStorage ->
            AuthenticationMsg (Authentication.BootFailure err)

        Just Alien.AuthTextile ->
            AuthenticationMsg (Authentication.BootFailure err)

        Just Alien.StoreTracksInCache ->
            case
                Json.Decode.decodeValue
                    (Json.Decode.list Json.Decode.string)
                    event.data
            of
                Ok trackIds ->
                    TracksMsg (Tracks.FailedToStoreInCache trackIds)

                Err _ ->
                    showErrorNotification err

        _ ->
            showErrorNotification err



-- ⚗️


showErrorNotification : String -> Msg
showErrorNotification =
    Notifications.error >> ShowNotification
