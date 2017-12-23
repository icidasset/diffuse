module Settings.State exposing (..)

import Element.Input as Input
import Response.Ext exposing (do)
import Settings.Types exposing (..)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { backgroundImage = Input.dropMenu (Just "7.jpg") SelectBackgroundImage
    }



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        SelectBackgroundImage selectMsg ->
            (!)
                { model | backgroundImage = Input.updateSelection selectMsg model.backgroundImage }
                [ do TopLevel.DebounceStoreUserData ]
