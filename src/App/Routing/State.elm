module Routing.State exposing (..)

import Navigation
import Routing.Logic as Logic
import Routing.Types exposing (..)


-- Initial


initialModel : Navigation.Location -> Model
initialModel location =
    { currentPage = Logic.locationToPage location }


initialCommands : Cmd Msg
initialCommands =
    Cmd.none



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage page ->
            (!) { model | currentPage = page } []
