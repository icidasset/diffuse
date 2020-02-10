module UI.Audio.State exposing (..)

import Dict
import LastFm
import Management
import Maybe.Extra as Maybe
import Monocle.Lens as Lens exposing (Lens)
import Return exposing (return)
import Return.Ext as Return exposing (communicate)
import UI.Audio.Types as Audio exposing (Msg(..))
import UI.Ports as Ports
import UI.Queue as Queue
import UI.Reply as Reply
import UI.Types as UI exposing (Manager, Organizer)



-- 🌳


initialModel : Audio.Model
initialModel =
    { duration = 0
    , hasStalled = False
    , isLoading = False
    , isPlaying = False
    , position = 0

    --
    , progress = Dict.empty
    , rememberProgress = True
    }


lens : Lens UI.Model Audio.Model
lens =
    { get = .audio
    , set = \audio ui -> { ui | audio = audio }
    }



-- 📣


update : Audio.Msg -> Manager
update msg =
    case msg of
        NoteProgress a ->
            organize (noteProgress a)

        PlayPause ->
            playPause

        SetDuration a ->
            setDuration a

        SetHasStalled a ->
            organize (setHasStalled a)

        SetIsLoading a ->
            organize (setIsLoading a)

        SetIsPlaying a ->
            organize (setIsPlaying a)

        SetPosition a ->
            organize (setPosition a)

        Stop ->
            stop


organize : Organizer Audio.Model -> Manager
organize =
    Management.organize lens



-- 📰


subscriptions : UI.Model -> Sub UI.Msg
subscriptions _ =
    [ Ports.noteProgress NoteProgress
    , Ports.requestPlayPause (always PlayPause)
    , Ports.requestStop (always Stop)
    , Ports.setAudioDuration SetDuration
    , Ports.setAudioHasStalled SetHasStalled
    , Ports.setAudioIsLoading SetIsLoading
    , Ports.setAudioIsPlaying SetIsPlaying
    , Ports.setAudioPosition SetPosition
    ]
        |> Sub.batch
        |> Sub.map UI.Audio



-- 🔱


noteProgress : { trackId : String, progress : Float } -> Organizer Audio.Model
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

    else if model.audio.isPlaying then
        communicate (Ports.pause ()) model

    else
        communicate (Ports.play ()) model


setDuration : Float -> Manager
setDuration duration model =
    let
        cmd =
            case model.tracks.nowPlaying of
                Just ( _, track ) ->
                    LastFm.nowPlaying model.lastFm
                        { duration = round duration
                        , msg = UI.Bypass
                        , track = track
                        }

                Nothing ->
                    Cmd.none
    in
    model
        |> Lens.modify lens (\audio -> { audio | duration = duration })
        |> Return.communicate cmd


setHasStalled : Bool -> Organizer Audio.Model
setHasStalled hasStalled model =
    Return.singleton { model | hasStalled = hasStalled }


setIsLoading : Bool -> Organizer Audio.Model
setIsLoading isLoading model =
    Return.singleton { model | isLoading = isLoading }


setIsPlaying : Bool -> Organizer Audio.Model
setIsPlaying isPlaying model =
    Return.singleton { model | isPlaying = isPlaying }


setPosition : Float -> Organizer Audio.Model
setPosition position model =
    Return.singleton { model | position = position }


stop : Manager
stop =
    communicate (Ports.pause ())
