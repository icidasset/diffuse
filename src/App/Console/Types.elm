module Console.Types exposing (..)

-- Types


type Msg
    = RequestPause
    | RequestPlay
    | Seek Float
    | SetDuration Float
    | SetIsPlaying Bool
    | SetStalled Bool
    | Unstall


{-| Model.

    Notes:
    - `duration` is in seconds

-}
type alias Model =
    { duration : Float
    , isPlaying : Bool
    , stalled : Bool
    }
