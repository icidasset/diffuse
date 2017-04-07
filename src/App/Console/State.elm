module Console.State exposing (..)

import Console.Types exposing (..)
import Console.Ports as Ports
import Console.Utils exposing (..)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    {}


initialCommands : Cmd Msg
initialCommands =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        Seek float ->
            ($) model [ Ports.requestSeek float ] []



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        []
