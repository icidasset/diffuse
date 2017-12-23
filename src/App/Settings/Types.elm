module Settings.Types exposing (..)

import Element.Input as Input


-- Messages


type Msg
    = SelectBackgroundImage (Input.SelectMsg String)



-- Model


type alias Model =
    { backgroundImage : Input.SelectWith String Msg
    }
