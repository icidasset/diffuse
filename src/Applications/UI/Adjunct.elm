module UI.Adjunct exposing (..)

import Keyboard
import Maybe.Extra as Maybe
import Return
import Return.Ext as Return
import UI.Alfred.State as Alfred
import UI.Audio.State as Audio
import UI.Authentication.Common as Authentication
import UI.Common.State as Common
import UI.Interface.State exposing (hideOverlay)
import UI.Page as Page
import UI.Playlists.Page as Playlists
import UI.Playlists.State as Playlists
import UI.Queue.Page as Queue
import UI.Queue.State as Queue
import UI.Settings.Page as Settings
import UI.Sources.Page as Sources
import UI.Tracks.State as Tracks
import UI.Types as UI exposing (..)



-- 📣


keyboardInput : Keyboard.Msg -> Manager
keyboardInput msg model =
    (\m ->
        let
            skip =
                Return.singleton m

            authenticated =
                Authentication.isAuthenticated model.authentication
        in
        if not authenticated || (m.focusedOnInput && Maybe.isNothing model.alfred) then
            case m.pressedKeys of
                [ Keyboard.Escape ] ->
                    hideOverlay m

                _ ->
                    skip

        else if Maybe.isJust model.alfred then
            case m.pressedKeys of
                [ Keyboard.ArrowDown ] ->
                    Alfred.selectNext m

                [ Keyboard.ArrowUp ] ->
                    Alfred.selectPrevious m

                [ Keyboard.Enter ] ->
                    Alfred.runSelectedAction m

                [ Keyboard.Escape ] ->
                    hideOverlay m

                _ ->
                    skip

        else
            case m.pressedKeys of
                [ Keyboard.Character "{", Keyboard.Shift ] ->
                    Queue.rewind m

                [ Keyboard.Character "}", Keyboard.Shift ] ->
                    Queue.shift m

                [ Keyboard.Character "<", Keyboard.Shift ] ->
                    Audio.seek ((m.audioPosition - 10) / m.audioDuration) m

                [ Keyboard.Character ">", Keyboard.Shift ] ->
                    Audio.seek ((m.audioPosition + 10) / m.audioDuration) m

                --
                [ Keyboard.Character "L" ] ->
                    Playlists.assistWithSelectingPlaylist m

                [ Keyboard.Character "N" ] ->
                    Tracks.scrollToNowPlaying m

                [ Keyboard.Character "P" ] ->
                    Audio.playPause m

                [ Keyboard.Character "R" ] ->
                    Queue.toggleRepeat m

                [ Keyboard.Character "S" ] ->
                    Queue.toggleShuffle m

                --
                [ Keyboard.Character "1" ] ->
                    Common.changeUrlUsingPage Page.Index m

                [ Keyboard.Character "2" ] ->
                    Common.changeUrlUsingPage (Page.Playlists Playlists.Index) m

                [ Keyboard.Character "3" ] ->
                    Common.changeUrlUsingPage (Page.Queue Queue.Index) m

                [ Keyboard.Character "4" ] ->
                    Common.changeUrlUsingPage Page.Equalizer m

                [ Keyboard.Character "8" ] ->
                    Common.changeUrlUsingPage (Page.Sources Sources.Index) m

                [ Keyboard.Character "9" ] ->
                    Common.changeUrlUsingPage (Page.Settings Settings.Index) m

                --
                [ Keyboard.Escape ] ->
                    hideOverlay m

                _ ->
                    skip
    )
        { model | pressedKeys = Keyboard.update msg model.pressedKeys }
