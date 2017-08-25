module Settings.State exposing (..)

import Settings.Ports as Ports
import Settings.Types exposing (..)
import Types as TopLevel exposing (ProgramFlags)


-- 💧


initialModel : ProgramFlags -> Model
initialModel flags =
    flags.settings.application



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        SetBackgroundImage filename ->
            let
                newModel =
                    { model | backgroundImage = filename }
            in
                (!)
                    newModel
                    [ Ports.storeApplicationSettings newModel ]
