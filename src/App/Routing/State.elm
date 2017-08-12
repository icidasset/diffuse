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


initialCommands : Navigation.Location -> Cmd TopLevel.Msg
initialCommands location =
    -- Trigger routing-transitions
    location
        |> Logic.locationToPage
        |> SetPage
        |> TopLevel.RoutingMsg
        |> do



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        GoToPage page ->
            (!)
                model
                [ Navigation.newUrl (Logic.pageToHref page) ]

        SetPage page ->
            (!)
                { model | currentPage = page }
                []
