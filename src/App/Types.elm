module Types exposing (..)

import BackgroundImage.Types as BackgroundImage
import Routing.Types as Routing


-- MESSAGES


type Msg
    = BackgroundImageMsg BackgroundImage.Msg
    | RoutingMsg Routing.Msg



-- MODEL


type alias Model =
    { showLoadingScreen : Bool
    , ------------------------------------
      -- Children
      ------------------------------------
      backgroundImage : BackgroundImage.Model
    , routing : Routing.Model
    }



-- FLAGS


type alias ProgramFlags =
    {}
