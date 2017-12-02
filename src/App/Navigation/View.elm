module Navigation.View exposing (..)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (href, title)
import Html.Events.Extra exposing (onClickPreventDefault)
import Navigation.Styles exposing (..)
import Navigation.Types exposing (..)
import Routing.Logic exposing (isSameBase, pageToHref)
import Routing.Types as Routing
import Types exposing (Msg(RoutingMsg))
import Utils exposing (cssClass)
import Variables exposing (colorDerivatives)


-- 🍯


outside : Routing.Page -> List ( Html Msg, Routing.Page ) -> Html Msg
outside currentPage items =
    div
        [ cssClass OutsideNavigation ]
        (List.map (itemViewWithActiveLink currentPage) items)


outsideOutgoing : Routing.Page -> List ( Html Msg, String ) -> Html Msg
outsideOutgoing currentPage items =
    div
        [ cssClass OutsideNavigation ]
        (List.map (itemViewOutgoing <| pageToHref currentPage) items)


inside : List ( Icon Msg, Label, Routing.Page ) -> Html Msg
inside items =
    div
        [ cssClass InsideNavigation ]
        (List.map itemView items)


insideCustom : List ( Icon Msg, Label, Msg ) -> Html Msg
insideCustom items =
    div
        [ cssClass InsideNavigation ]
        (List.map itemViewCustom items)



-- Items


itemView : ( Icon Msg, Label, Routing.Page ) -> Html Msg
itemView ( icon, label, itemPage ) =
    itemPage
        |> Routing.GoToPage
        |> RoutingMsg
        |> (,,) icon label
        |> itemViewCustom


itemViewCustom : ( Icon Msg, Label, Msg ) -> Html Msg
itemViewCustom ( Icon icon, Label label, msg ) =
    let
        maybeHref =
            case msg of
                RoutingMsg (Routing.GoToPage page) ->
                    Just (pageToHref page)

                _ ->
                    Nothing

        baseAttr =
            [ onClickPreventDefault msg

            --
            , case label of
                Hidden _ ->
                    cssClass NonFlexLink

                Shown _ ->
                    cssClass FlexLink

            --
            , case label of
                Hidden l ->
                    title l

                Shown _ ->
                    title ""
            ]
    in
        a
            (case maybeHref of
                Just it ->
                    href it :: baseAttr

                Nothing ->
                    baseAttr
            )
            [ span
                []
                [ icon colorDerivatives.text 16
                , case label of
                    Hidden _ ->
                        Html.text ""

                    Shown l ->
                        Html.label [] [ text l ]
                ]
            ]



-- Items, Pt 2.


itemViewWithActiveLink : Routing.Page -> ( Html Msg, Routing.Page ) -> Html Msg
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


itemViewOutgoing : String -> ( Html Msg, String ) -> Html Msg
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
