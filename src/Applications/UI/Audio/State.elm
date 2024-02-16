module UI.Audio.State exposing (..)

import Dict
import LastFm
import Maybe.Extra as Maybe
import Queue
import Return exposing (return)
import Return.Ext as Return exposing (communicate)
import Tracks exposing (Track)
import UI.Audio.Types exposing (..)
import UI.Ports as Ports
import UI.Queue.State as Queue
import UI.Types as UI exposing (Manager, Model)
import UI.User.State.Export as User



-- 📣


canPlay : { trackId : String, duration : Float } -> Manager
canPlay { trackId, duration } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying ->
            replaceNowPlaying { nowPlaying | duration = Just duration }
        )


ended : { trackId : String } -> Manager
ended { trackId } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\_ model ->
            if model.repeat then
                play model

            else
                Queue.shift model
        )


hasLoaded : { trackId : String } -> Manager
hasLoaded { trackId } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying ->
            replaceNowPlaying { nowPlaying | loadingState = Loaded }
        )


hasStalled : { trackId : String } -> Manager
hasStalled { trackId } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying ->
            replaceNowPlaying { nowPlaying | loadingState = Stalled }
        )


isLoading : { trackId : String } -> Manager
isLoading { trackId } =
    -- TODO: Debounce?
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying ->
            replaceNowPlaying { nowPlaying | loadingState = Loading }
        )


noteProgress : { trackId : String, progress : Float } -> Manager
noteProgress { trackId, progress } model =
    let
        updatedProgressTable =
            if not model.rememberProgress then
                model.progress

            else if progress > 0.975 then
                Dict.remove trackId model.progress

            else
                Dict.insert trackId progress model.progress
    in
    if model.rememberProgress then
        User.saveProgress { model | progress = updatedProgressTable }

    else
        Return.singleton model


pause : Manager
pause model =
    case model.nowPlaying of
        Just { item } ->
            communicate
                (Ports.pause
                    { trackId = (Tuple.second item.identifiedTrack).id
                    }
                )
                model

        Nothing ->
            -- TODO?
            Return.singleton model


playbackStateChanged : { trackId : String, isPlaying : Bool } -> Manager
playbackStateChanged { trackId, isPlaying } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying ->
            replaceNowPlaying { nowPlaying | isPlaying = isPlaying }
        )


playPause : Manager
playPause model =
    case model.nowPlaying of
        Just { isPlaying } ->
            if isPlaying then
                pause model

            else
                play model

        Nothing ->
            play model


play : Manager
play model =
    case model.nowPlaying of
        Just { item } ->
            communicate
                (Ports.play
                    { trackId = (Tuple.second item.identifiedTrack).id
                    , volume = model.eqSettings.volume
                    }
                )
                model

        Nothing ->
            Queue.shift model


seek : { trackId : String, progress : Float } -> Manager
seek { trackId, progress } =
    { percentage = progress, trackId = trackId }
        |> Ports.seek
        |> Return.communicate



-- TODO


setDuration : Float -> Manager
setDuration duration model =
    -- TODO:
    -- let
    --     cmd =
    --         case Maybe.map (.item >> .identifiedTrack) model.nowPlaying of
    --             Just ( _, track ) ->
    --                 LastFm.nowPlaying model.lastFm
    --                     { duration = round duration
    --                     , msg = UI.Bypass
    --                     , track = track
    --                     }
    --             Nothing ->
    --                 Cmd.none
    -- in
    -- return
    --   { model | audioPlaybackState =
    --       Maybe.map
    --         (\state -> {state|duration=Just duration})
    --         model.audioPlaybackState
    --       }
    --   cmd
    Return.singleton model


stop : Manager
stop =
    -- TODO:
    Return.singleton


toggleRememberProgress : Manager
toggleRememberProgress model =
    User.saveSettings { model | rememberProgress = not model.rememberProgress }



-- 🛠️


onlyIfMatchesNowPlaying : { trackId : String } -> (NowPlaying -> Manager) -> Manager
onlyIfMatchesNowPlaying { trackId } fn model =
    case model.nowPlaying of
        Just ({ item } as nowPlaying) ->
            if trackId == (Tuple.second item.identifiedTrack).id then
                fn nowPlaying model

            else
                Return.singleton model

        Nothing ->
            Return.singleton model


replaceNowPlaying : NowPlaying -> Manager
replaceNowPlaying np model =
    Return.singleton { model | nowPlaying = Just np }
