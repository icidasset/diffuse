module Routing.State exposing (..)

import Navigation
import Response.Ext exposing (do)
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
            let
                cmd =
                    if model.currentPage == Index then
                        do TopLevel.RecalibrateTracks
                    else
                        Cmd.none
            in
                (,) { model | currentPage = page } cmd

        GoToUrl url ->
            (,) model (Navigation.newUrl url)
