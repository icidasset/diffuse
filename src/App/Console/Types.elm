module Console.Types exposing (..)

-- Types


type Msg
    = RequestPause
    | RequestPlay
    | Seek Float
    | SetDuration Float
    | SetIsPlaying Bool


{-| Model.

    Notes:
    - `duration` is in seconds
    - `progress` is a percentage, so the currentTime = duration * percentage
-}
type alias Model =
    { duration : Float
    , isPlaying : Bool
    }
