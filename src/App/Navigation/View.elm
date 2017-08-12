module Navigation.View exposing (..)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (href)
import Html.Events.Extra exposing (onClickPreventDefault)
import Navigation.Styles exposing (..)
import Routing.Logic exposing (isSameBase, pageToHref)
import Routing.Types as Routing
import Types exposing (Model, Msg(RoutingMsg))
import Utils exposing (cssClass)


-- 🍯


outside : Routing.Page -> List ( Label, Routing.Page ) -> Html Msg
outside currentPage items =
    div
        [ cssClass OutsideNavigation ]
        (List.map (itemViewWithActiveLink currentPage) items)


outsideOutgoing : Routing.Page -> List ( Label, String ) -> Html Msg
outsideOutgoing currentPage items =
    div
        [ cssClass OutsideNavigation ]
        (List.map (itemViewOutgoing <| pageToHref currentPage) items)


inside : List ( Label, Routing.Page ) -> Html Msg
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


itemView : ( Label, Routing.Page ) -> Html Msg
itemView ( itemLabel, itemPage ) =
    a
        [ href (pageToHref itemPage)
        , onClickPreventDefault (RoutingMsg <| Routing.GoToPage itemPage)
        ]
        [ span
            []
            [ itemLabel ]
        ]


itemViewWithActiveLink : Routing.Page -> ( Label, Routing.Page ) -> Html Msg
itemViewWithActiveLink currentPage ( itemLabel, itemPage ) =
    a
        [ href (pageToHref itemPage)
        , onClickPreventDefault (RoutingMsg <| Routing.GoToPage itemPage)

        --
        , if isSameBase currentPage itemPage then
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


itemViewOutgoing : String -> ( Label, String ) -> Html Msg
itemViewOutgoing activeHref ( itemLabel, itemHref ) =
    a
        [ href itemHref
        , if itemHref == activeHref then
            cssClass ActiveLink
          else
            cssClass NonActiveLink
        ]
        [ span
            []
            [ itemLabel ]
        ]
