module UI.Audio.State exposing (..)

import Dict
import LastFm
import Maybe.Extra as Maybe
import Return exposing (return)
import Return.Ext as Return exposing (communicate)
import UI.Ports as Ports
import UI.Queue.State as Queue
import UI.Types as UI exposing (Manager)
import UI.User.State.Export as User



-- 📣


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


playPause : Manager
playPause model =
    if Maybe.isNothing model.nowPlaying then
        Queue.shift model

    else if model.audioIsPlaying then
        communicate (Ports.pause ()) model

    else
        communicate (Ports.play ()) model


seek : Float -> Manager
seek percentage =
    Return.communicate (Ports.seek percentage)


setDuration : Float -> Manager
setDuration duration model =
    let
        cmd =
            case Maybe.map .identifiedTrack model.nowPlaying of
                Just ( _, track ) ->
                    LastFm.nowPlaying model.lastFm
                        { duration = round duration
                        , msg = UI.Bypass
                        , track = track
                        }

                Nothing ->
                    Cmd.none
    in
    return { model | audioDuration = duration } cmd


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


toggleRememberProgress : Manager
toggleRememberProgress model =
    User.saveSettings { model | rememberProgress = not model.rememberProgress }
