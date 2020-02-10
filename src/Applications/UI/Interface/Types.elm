module UI.Interface.Types exposing (..)

import Common exposing (Switch)
import Debouncer.Basic as Debouncer exposing (Debouncer)
import Keyboard
import Notifications exposing (Notification)
import Time
import UI.Reply exposing (Reply)



-- 📣


type Msg
    = Blur
    | Debounce (Debouncer.Msg Msg)
    | FocusedOnInput
    | HideOverlay
    | KeyboardMsg Keyboard.Msg
    | PreferredColorSchemaChanged { dark : Bool }
    | RemoveQueueSelection
    | RemoveTrackSelection
    | ResizedWindow ( Int, Int )
    | ShowNotification (Notification Reply)
    | SetCurrentTime Time.Posix
    | SetIsOnline Bool
    | SetIsTouchDevice Bool
    | StoppedDragging
    | ToggleLoadingScreen Switch
