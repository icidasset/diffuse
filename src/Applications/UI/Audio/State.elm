module UI.Audio.State exposing (..)

import Debouncer.Basic as Debouncer exposing (Debouncer)
import Dict
import LastFm
import Maybe.Extra as Maybe
import MediaSession
import Queue
import Return exposing (return)
import Return.Ext as Return exposing (communicate)
import Tracks exposing (Track)
import UI.Audio.Types exposing (..)
import UI.Common.State as Common
import UI.Common.Types exposing (DebounceManager)
import UI.Ports as Ports
import UI.Queue.State as Queue
import UI.Types as UI exposing (Manager, Model, Msg(..))
import UI.User.State.Export as User



-- 📣  ░░  EVENTS


durationChange : DurationChangeEvent -> Manager
durationChange { trackId, duration } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying model ->
            let
                ( _, track ) =
                    nowPlaying.item.identifiedTrack

                tags =
                    { album = track.tags.album
                    , artist = track.tags.artist
                    , title = track.tags.title
                    }
            in
            model
                |> replaceNowPlaying { nowPlaying | duration = Just duration }
                |> Return.command (Ports.setMediaSessionMetadata tags)
                |> Return.command (Ports.resetScrobbleTimer { duration = duration, trackId = trackId })
                |> Return.andThen (notifyScrobblersOfTrackPlaying { duration = duration })
        )

error : ErrorAudioEvent -> Manager
error { trackId, code } =
  onlyIfMatchesNowPlaying
      { trackId = trackId }
      (\nowPlaying ->
          replaceNowPlaying (
            case code of
              2 ->
                { nowPlaying | loadingState = NetworkError }
              3 ->
                { nowPlaying | loadingState = DecodeError }
              4 ->
                { nowPlaying | loadingState = NotSupportedError }
              _ ->
                nowPlaying
          )
      )


ended : GenericAudioEvent -> Manager
ended { trackId } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying model ->
            if model.repeat then
                Return.command
                    (case nowPlaying.duration of
                        Just duration ->
                            Ports.resetScrobbleTimer { duration = duration, trackId = trackId }

                        Nothing ->
                            Cmd.none
                    )
                    (play model)

            else
                Return.andThen
                    (if Maybe.map (\d -> Tracks.shouldNoteProgress { duration = d }) nowPlaying.duration == Just True then
                        noteProgress { trackId = trackId, progress = 1.0 }

                     else
                        Return.singleton
                    )
                    (Queue.shift model)
        )


hasLoaded : GenericAudioEvent -> Manager
hasLoaded { trackId } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying ->
            replaceNowPlaying { nowPlaying | loadingState = Loaded }
        )


isLoading : GenericAudioEvent -> Manager
isLoading { trackId } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying ->
            replaceNowPlaying { nowPlaying | loadingState = Loading }
        )


playbackStateChanged : PlaybackStateEvent -> Manager
playbackStateChanged { trackId, isPlaying } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying model ->
            { model | nowPlaying = Just { nowPlaying | isPlaying = isPlaying } }
                |> Return.singleton
                |> Return.command
                    (if isPlaying then
                        Ports.startScrobbleTimer ()

                     else
                        Ports.pauseScrobbleTimer ()
                    )
                |> Return.command
                    (Ports.setMediaSessionPlaybackState
                        (if isPlaying then
                            MediaSession.states.playing

                         else
                            MediaSession.states.paused
                        )
                    )
        )


timeUpdated : TimeUpdatedEvent -> Manager
timeUpdated { trackId, currentTime, duration } =
    onlyIfMatchesNowPlaying
        { trackId = trackId }
        (\nowPlaying model ->
            let
              dur =
                Maybe.withDefault 0 duration
            in
            { model | nowPlaying = Just { nowPlaying | duration = duration, playbackPosition = currentTime } }
                |> (if Tracks.shouldNoteProgress { duration = dur } then
                        { trackId = trackId
                        , progress = currentTime / dur
                        }
                            |> NoteProgress
                            |> Debouncer.provideInput
                            |> NoteProgressDebounce
                            |> Return.task
                            |> Return.communicate

                    else
                        Return.singleton
                   )
                |> Return.command
                    (case duration of
                        Just d ->
                          Ports.setMediaSessionPositionState
                            { currentTime = currentTime
                            , duration = d
                            }
                        Nothing -> Cmd.none
                    )
        )



-- 📣  ░░  COMMANDS


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
            Return.singleton model


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


stop : Manager
stop model =
    model.audioElements
        |> List.filter (.isPreload >> (==) True)
        |> (\a -> { model | audioElements = a })
        |> Queue.changeActiveItem Nothing
        |> Return.effect_
            (\m ->
                Ports.renderAudioElements
                    { items = m.audioElements
                    , play = Nothing
                    , volume = m.eqSettings.volume
                    }
            )



-- 📣


noteProgress : { trackId : String, progress : Float } -> Manager
noteProgress { trackId, progress } model =
    let
        _ =
            Debug.log "noteProgress" { trackId = trackId, progress = progress }

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


noteProgressDebounce : DebounceManager
noteProgressDebounce =
    Common.debounce
        .progressDebouncer
        (\d m -> { m | progressDebouncer = d })
        UI.NoteProgressDebounce


notifyScrobblersOfTrackPlaying : { duration : Float } -> Manager
notifyScrobblersOfTrackPlaying { duration } model =
    case model.nowPlaying of
        Just { item } ->
            { duration = round duration
            , msg = UI.Bypass
            , track = Tuple.second item.identifiedTrack
            }
                |> LastFm.nowPlaying model.lastFm
                |> return model

        Nothing ->
            Return.singleton model


preloadDebounce : DebounceManager
preloadDebounce =
    Common.debounce
        .preloadDebouncer
        (\d m -> { m | preloadDebouncer = d })
        UI.AudioPreloadDebounce


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
