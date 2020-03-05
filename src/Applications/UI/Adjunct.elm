module UI.Adjunct exposing (..)

import Keyboard
import Maybe.Extra as Maybe
import Return
import Return.Ext as Return exposing (communicate)
import UI.Alfred.State as Alfred
import UI.Authentication as Authentication
import UI.Interface.State exposing (hideOverlay)
import UI.Reply as Reply exposing (Reply)
import UI.Types as UI exposing (..)



-- 📣


keyboardInput : Keyboard.Msg -> Manager
keyboardInput msg model =
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

        else if Maybe.isJust model.alfred then
            case m.pressedKeys of
                [ Keyboard.ArrowDown ] ->
                    Alfred.selectPrevious m

                [ Keyboard.ArrowUp ] ->
                    Alfred.selectNext m

                [ Keyboard.Enter ] ->
                    Alfred.runSelectedAction m

                _ ->
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
                    Return.performance (Reply (Reply.Seek <| (m.audioPosition - 10) / m.audioDuration)) m

                [ Keyboard.ArrowDown ] ->
                    Return.performance (Reply (Reply.Seek <| (m.audioPosition + 10) / m.audioDuration)) m

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
