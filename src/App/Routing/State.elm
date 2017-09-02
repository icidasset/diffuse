module Routing.State exposing (..)

import Navigation
import Response.Ext exposing (do)
import Routing.Logic as Logic
import Routing.Types exposing (..)
import Types as TopLevel


-- 💧


initialModel : Page -> Model
initialModel initialPage =
    { currentPage = initialPage
    }


initialCommands : Page -> Cmd TopLevel.Msg
initialCommands initialPage =
    -- Trigger routing-transitions
    initialPage
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
