module UI.View exposing (view)

import Browser
import UI.Theme
import UI.Types exposing (Model, Msg)



-- 🗺


view : Model -> Browser.Document Msg
view model =
    { title = "Diffuse"
    , body = [ UI.Theme.view model ]
    }
