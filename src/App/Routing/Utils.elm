module Routing.Utils exposing (..)

import Response.Ext exposing (do)
import Routing.Types exposing (Msg(GoToPage), Page)
import Types as TopLevel


goTo : Page -> Cmd TopLevel.Msg
goTo page =
    page
        |> GoToPage
        |> TopLevel.RoutingMsg
        |> do
