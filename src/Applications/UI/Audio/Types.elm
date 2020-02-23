module UI.Audio.Types exposing (..)

import Dict exposing (Dict)



-- 🌳


type alias Model =
    { duration : Float
    , hasStalled : Bool
    , isLoading : Bool
    , isPlaying : Bool
    , position : Float

    -----------------------------------------
    -- Progress
    -----------------------------------------
    , progress : Dict String Float
    , rememberProgress : Bool
    }



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
