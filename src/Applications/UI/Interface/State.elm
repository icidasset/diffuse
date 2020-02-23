module UI.Interface.State exposing (..)

import Alien
import Browser.Events
import Common exposing (Switch(..))
import Debouncer.Basic as Debouncer
import Keyboard
import Management
import Maybe.Extra as Maybe
import Monocle.Lens as Lens
import Notifications
import Return exposing (return)
import Return.Ext as Return exposing (communicate)
import Time
import UI.Authentication as Authentication
import UI.Common.State as Common exposing (modifySingleton)
import UI.DnD as DnD
import UI.Page as Page
import UI.Ports as Ports
import UI.Queue as Queue
import UI.Queue.State as Queue
import UI.Reply as Reply
import UI.Sources.State as Sources
import UI.Tracks as Tracks
import UI.Tracks.Scene.List
import UI.Tracks.State as Tracks
import UI.Types as UI exposing (..)
import User.Layer exposing (..)



-- 📣


blur : UI.Manager
blur model =
    Return.singleton { model | focusedOnInput = False }


debounce : (Msg -> Model -> ( Model, Cmd Msg )) -> Debouncer.Msg Msg -> UI.Manager
debounce update debouncerMsg model =
    let
        ( subModel, subCmd, emittedMsg ) =
            Debouncer.update debouncerMsg model.debounce

        mappedCmd =
            Cmd.map Debounce subCmd

        updatedModel =
            { model | debounce = subModel }
    in
    case emittedMsg of
        Just emitted ->
            updatedModel
                |> update emitted
                |> Return.command mappedCmd

        Nothing ->
            return updatedModel mappedCmd


focusedOnInput : UI.Manager
focusedOnInput model =
    Return.singleton { model | focusedOnInput = True }


hideOverlay : UI.Manager
hideOverlay model =
    Return.singleton
        { model
            | alfred = { instance = Nothing }
            , confirmation = Nothing
            , contextMenu = Nothing
        }


keyboardMsg : Keyboard.Msg -> UI.Manager
keyboardMsg msg model =
    (\m ->
        let
            skip =
                Return.singleton m

            authenticated =
                case model.authentication of
                    Authentication.Authenticated _ ->
                        True

                    _ ->
                        False
        in
        if m.focusedOnInput || not authenticated then
            -- Stop here if using input or not authenticated
            skip

        else
            case m.pressedKeys of
                [ Keyboard.Escape ] ->
                    hideOverlay m

                [ Keyboard.ArrowLeft ] ->
                    Return.performance (Reply Reply.RewindQueue) m

                [ Keyboard.ArrowRight ] ->
                    Return.performance (Reply Reply.ShiftQueue) m

                [ Keyboard.ArrowUp ] ->
                    Return.performance (Reply (Reply.Seek <| (m.audio.position - 10) / m.audio.duration)) m

                [ Keyboard.ArrowDown ] ->
                    Return.performance (Reply (Reply.Seek <| (m.audio.position + 10) / m.audio.duration)) m

                [ Keyboard.Character "N" ] ->
                    Return.performance (Reply Reply.ScrollToNowPlaying) m

                [ Keyboard.Character "P" ] ->
                    Return.performance (Reply Reply.TogglePlayPause) m

                [ Keyboard.Character "R" ] ->
                    Return.performance (Reply Reply.ToggleRepeat) m

                [ Keyboard.Character "S" ] ->
                    Return.performance (Reply Reply.ToggleShuffle) m

                _ ->
                    skip
    )
        { model | pressedKeys = Keyboard.update msg model.pressedKeys }


preferredColorSchemaChanged : { dark : Bool } -> UI.Manager
preferredColorSchemaChanged { dark } model =
    Return.singleton { model | darkMode = dark }


removeQueueSelection : UI.Manager
removeQueueSelection =
    modifySingleton Queue.lens (\q -> { q | selection = Nothing })


removeTrackSelection : UI.Manager
removeTrackSelection =
    modifySingleton Tracks.lens (\t -> { t | selectedTrackIndexes = [] })


resizedWindow : ( Int, Int ) -> UI.Manager
resizedWindow ( width, height ) model =
    Return.singleton
        { model
            | contextMenu = Nothing
            , viewport = { height = toFloat height, width = toFloat width }
        }


setIsTouchDevice : Bool -> UI.Manager
setIsTouchDevice bool model =
    Return.singleton { model | isTouchDevice = bool }


stoppedDragging : UI.Manager
stoppedDragging model =
    let
        notDragging =
            { model | isDragging = False }
    in
    -- Depending on where we stopped dragging something,
    -- do the appropriate thing.
    case model.page of
        Page.Queue _ ->
            -- TODO!
            DnD.stoppedDragging
                |> Queue.DragMsg
                |> QueueMsg
                |> Return.performanceF notDragging

        Page.Index ->
            case model.tracks.scene of
                Tracks.List ->
                    -- TODO!
                    DnD.stoppedDragging
                        |> UI.Tracks.Scene.List.DragAndDropMsg
                        |> Tracks.ListSceneMsg
                        |> TracksMsg
                        |> Return.performanceF notDragging

        _ ->
            Return.singleton notDragging


toggleLoadingScreen : Switch -> UI.Manager
toggleLoadingScreen switch model =
    case switch of
        On ->
            Return.singleton { model | isLoading = True }

        Off ->
            Return.singleton { model | isLoading = False }
