module UI.Services.State exposing (..)

import Http
import LastFm
import Notifications
import Return exposing (andThen)
import Return.Ext as Return
import UI.Common.State as Common exposing (showNotification)
import UI.Reply exposing (Reply(..))
import UI.Types as UI exposing (Manager, Msg(..))



-- 🔱


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
                    (Return.performance <| Reply SaveSettings)


scrobble : { duration : Int, timestamp : Int, trackId : String } -> Manager
scrobble { duration, timestamp, trackId } model =
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
                Return.singleton model

        Nothing ->
            Return.singleton model
