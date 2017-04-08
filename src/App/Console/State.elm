module Console.State exposing (..)

import Console.Types exposing (..)
import Console.Ports as Ports
import Console.Utils exposing (..)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { duration = 0
    , isPlaying = False
    , progress = 0
    }


initialCommands : Cmd Msg
initialCommands =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        RequestPause ->
            ($) model [ Ports.requestPause () ] []

        RequestPlay ->
            ($) model [ Ports.requestPlay () ] []

        Seek float ->
            ($) model [ Ports.requestSeek float ] []

        SetDuration duration ->
            ($) { model | duration = duration } [] []

        SetIsPlaying bool ->
            ($) { model | isPlaying = bool } [] []

        SetProgress progress ->
            ($) { model | progress = progress } [] []



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.setDuration SetDuration
        , Ports.setIsPlaying SetIsPlaying
        , Ports.setProgress SetProgress
        ]
