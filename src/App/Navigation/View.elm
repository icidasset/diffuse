module Navigation.View exposing (..)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (href)
import Html.Events.Extra exposing (onClickPreventDefault)
import Navigation.Styles exposing (..)
import Routing.Logic exposing (pageToParentHref)
import Routing.Types as Routing
import Types exposing (Model, Msg(RoutingMsg))
import Utils exposing (cssClass)


-- 🍯


outside : Model -> List ( Label, String ) -> Html Msg
outside model items =
    let
        currentHref =
            pageToParentHref model.routing.currentPage
    in
        div
            [ cssClass OutsideNavigation ]
            (List.map (itemViewWithActiveLink currentHref) items)


inside : List ( Label, String ) -> Html Msg
inside items =
    div
        [ cssClass InsideNavigation ]
        (List.map itemView items)


insideCustom : List ( Label, Msg ) -> Html Msg
insideCustom items =
    div
        [ cssClass InsideNavigation ]
        (List.map itemViewCustom items)



-- Utility types


type alias Label =
    Html Msg



-- Items


itemView : ( Label, String ) -> Html Msg
itemView ( itemLabel, itemHref ) =
    a
        [ href itemHref
        , onClickPreventDefault (RoutingMsg <| Routing.GoToUrl itemHref)
        ]
        [ span
            []
            [ itemLabel ]
        ]


itemViewWithActiveLink : String -> ( Label, String ) -> Html Msg
itemViewWithActiveLink activeHref ( itemLabel, itemHref ) =
    a
        [ href itemHref
        , onClickPreventDefault (RoutingMsg <| Routing.GoToUrl itemHref)

        --
        , if itemHref == activeHref then
            cssClass ActiveLink
          else
            cssClass NonActiveLink
        ]
        [ span
            []
            [ itemLabel ]
        ]


itemViewCustom : ( Label, Msg ) -> Html Msg
itemViewCustom ( itemLabel, msg ) =
    a
        [ onClickPreventDefault msg ]
        [ span
            []
            [ itemLabel ]
        ]
