module Console.Types exposing (..)

-- Messages


type Msg
    = RequestPause
    | RequestPlay
    | Seek Float
    | SetDuration Float
    | SetIsLoading Bool
    | SetIsPlaying Bool
    | SetStalled Bool
    | Unstall


{-| Model.

    Notes:
    - `duration` is in seconds

-}
type alias Model =
    { duration : Float
    , isLoading : Bool
    , isPlaying : Bool
    , stalled : Bool
    }
