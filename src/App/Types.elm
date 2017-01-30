module Types exposing (..)

import Routing.Types as Routing


type Msg
    = RoutingMsg Routing.Msg


type alias Model =
    { backgroundImage : String
    , routing : Routing.Model
    }


type alias ProgramFlags =
    {}
