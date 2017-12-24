module Settings.State exposing (..)

import Response.Ext exposing (do)
import Settings.Types exposing (..)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { backgroundImage = "7.jpg"
    }



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        SetBackgroundImage filename ->
            (!)
                { model | backgroundImage = filename }
                [ do TopLevel.DebounceStoreUserData ]
