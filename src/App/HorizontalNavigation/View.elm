module HorizontalNavigation.View exposing (..)

import HorizontalNavigation.Styles exposing (..)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (href)
import Html.Events.Extra exposing (onClickPreventDefault)
import Routing.Types as Routing
import Types exposing (Msg(RoutingMsg))
import Utils exposing (cssClass)


-- 🍯


entry : List ( String, String ) -> Html Msg
entry items =
    div
        [ cssClass HorizontalNavigation ]
        (items
            |> List.map itemView
            |> List.map (Html.map RoutingMsg)
        )



-- Items


itemView : ( String, String ) -> Html Routing.Msg
itemView ( itemLabel, itemHref ) =
    a
        [ href itemHref, onClickPreventDefault (Routing.GoToUrl itemHref) ]
        [ span [] [ text itemLabel ] ]
