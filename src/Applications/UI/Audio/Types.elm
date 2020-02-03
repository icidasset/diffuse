module UI.Audio.Types exposing (..)

-- 📣


type Msg
    = NoteProgress { trackId : String, progress : Float }
    | PlayPause
    | SetDuration Float
    | SetHasStalled Bool
    | SetIsLoading Bool
    | SetIsPlaying Bool
    | SetPosition Float
    | Stop
