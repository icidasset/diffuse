module UI.Services.State exposing (..)

import Browser.Navigation as Nav
import Common
import Http
import LastFm
import Notifications
import Return exposing (andThen, return)
import String.Ext as String
import UI.Common.State exposing (showNotification)
import UI.Types exposing (Manager, Msg(..))
import UI.User.State.Export as User
import Url



-- 🔱


connectLastFm : Manager
connectLastFm model =
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


disconnectLastFm : Manager
disconnectLastFm model =
    User.saveSettings { model | lastFm = LastFm.disconnect model.lastFm }


gotLastFmSession : Result Http.Error String -> Manager
gotLastFmSession result model =
    case result of
        Err _ ->
            showNotification
                (Notifications.stickyError "Could not connect with Last.fm")
                { model | lastFm = LastFm.failedToAuthenticate model.lastFm }

        Ok sessionKey ->
            { model | lastFm = LastFm.gotSessionKey sessionKey model.lastFm }
                |> showNotification
                    (Notifications.success "Connected successfully with Last.fm")
                |> andThen
                    User.saveSettings


scrobble : { duration : Int, timestamp : Int, trackId : String } -> Manager
scrobble { duration, timestamp, trackId } model =
    case Maybe.map (.item >> .identifiedTrack) model.nowPlaying of
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
                Return.singleton model

        Nothing ->
            Return.singleton model
