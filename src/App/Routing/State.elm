module Routing.State exposing (..)

import Navigation
import Routing.Logic as Logic
import Routing.Types exposing (..)
import Types as TopLevel


-- 💧


initialModel : Navigation.Location -> Model
initialModel location =
    { currentPage = Logic.locationToPage location }


initialCommands : Cmd TopLevel.Msg
initialCommands =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        GoToPage page ->
            (!) { model | currentPage = page } []

        GoToUrl url ->
            (!) model [ Navigation.newUrl url ]
