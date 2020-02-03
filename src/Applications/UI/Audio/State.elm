module UI.Audio.State exposing (..)

import Dict
import LastFm
import Maybe.Extra as Maybe
import Return exposing (return)
import Return.Ext as Return exposing (communicate)
import UI.Audio.Types as Audio exposing (..)
import UI.Ports as Ports
import UI.Queue as Queue
import UI.Reply as Reply
import UI.Types as UI exposing (Manager)



-- 📣


update : Audio.Msg -> Manager
update msg =
    case msg of
        NoteProgress a ->
            noteProgress a

        PlayPause ->
            playPause

        SetDuration a ->
            setDuration a

        SetHasStalled a ->
            setHasStalled a

        SetIsLoading a ->
            setIsLoading a

        SetIsPlaying a ->
            setIsPlaying a

        SetPosition a ->
            setPosition a

        Stop ->
            stop



-- 🔱


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
        -- TODO!
        Return.performance
            (UI.Reply Reply.SaveProgress)
            { model | progress = updatedProgressTable }

    else
        Return.singleton model


playPause : Manager
playPause model =
    if Maybe.isNothing model.queue.activeItem then
        -- TODO!
        Return.performance (UI.QueueMsg Queue.Shift) model

    else if model.audioIsPlaying then
        communicate (Ports.pause ()) model

    else
        communicate (Ports.play ()) model


setDuration : Float -> Manager
setDuration duration model =
    (case model.tracks.nowPlaying of
        Just ( _, track ) ->
            { duration = round duration
            , msg = UI.Bypass
            , track = track
            }
                |> LastFm.nowPlaying model.lastFm
                |> communicate

        Nothing ->
            Return.singleton
    )
        { model | audioDuration = duration }


setHasStalled : Bool -> Manager
setHasStalled hasStalled model =
    Return.singleton { model | audioHasStalled = hasStalled }


setIsLoading : Bool -> Manager
setIsLoading isLoading model =
    Return.singleton { model | audioIsLoading = isLoading }


setIsPlaying : Bool -> Manager
setIsPlaying isPlaying model =
    Return.singleton { model | audioIsPlaying = isPlaying }


setPosition : Float -> Manager
setPosition position model =
    Return.singleton { model | audioPosition = position }


stop : Manager
stop =
    communicate (Ports.pause ())
