module Sources.View exposing (..)

import HorizontalNavigation.View as HorizontalNavigation
import Html exposing (Html, div, text)
import Routing.Types as Routing
import Sources.Types as Sources exposing (Page(..))
import Types exposing (Msg)


-- 🍯


entry : Sources.Page -> Html Msg
entry page =
    case page of
        Index ->
            text "Sources index"

        New ->
            div
                []
                [ HorizontalNavigation.entry
                    [ ( "Show my sources", "/sources" )
                    ]
                , text ""
                ]
