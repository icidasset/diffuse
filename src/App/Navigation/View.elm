module Navigation.View exposing (outside, outsideOutgoing, inside, insideCustom)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onWithOptions)
import Element.Events.Ext exposing (preventDefaultOptions)
import Element.Types exposing (Node)
import Json.Decode
import Navigation.Styles exposing (Styles(..))
import Navigation.Types exposing (..)
import Routing.Logic exposing (isSameBase, pageToHref)
import Routing.Types as Routing
import Styles exposing (Styles(Navigation, Zed))
import Types exposing (Msg(RoutingMsg))
import Variables exposing (colorDerivatives, scaled, scaledPx)
import Variations exposing (Variations(Active))


-- 🍯


outside : Routing.Page -> List ( String, Routing.Page ) -> Node
outside currentPage items =
    let
        mapFn =
            outsideView currentPage
    in
        row
            (Navigation Outside)
            [ center, spacing (scaled 8), width fill ]
            (List.map mapFn items)


outsideOutgoing : Routing.Page -> List ( Icon Msg, String ) -> Node
outsideOutgoing currentPage items =
    let
        mapFn =
            currentPage
                |> pageToHref
                |> outsideOutgoingView
    in
        row
            (Navigation Outside)
            [ center, spacing (scaled 8), width fill ]
            (List.map mapFn items)


inside : List ( Icon Msg, Label, Routing.Page ) -> Node
inside items =
    items
        |> List.map insideView
        |> row (Navigation Inside) []


insideCustom : List ( Icon Msg, Label, Msg ) -> Node
insideCustom items =
    items
        |> List.map insideViewCustom
        |> row (Navigation Inside) []



-- Outside


outsideView : Routing.Page -> ( String, Routing.Page ) -> Node
outsideView currentPage ( itemLabel, itemPage ) =
    itemLabel
        |> text
        |> el
            (Navigation OutsideItem)
            [ vary Active (isSameBase currentPage itemPage)

            -- Events
            , itemPage
                |> Routing.GoToPage
                |> RoutingMsg
                |> Json.Decode.succeed
                |> onWithOptions "click" preventDefaultOptions
            ]
        |> link (pageToHref itemPage)


outsideOutgoingView : String -> ( Icon Msg, String ) -> Node
outsideOutgoingView activeHref ( Icon icon, itemHref ) =
    icon colorDerivatives.text 16
        |> html
        |> el (Navigation OutsideItem) [ vary Active (itemHref == activeHref) ]
        |> link itemHref



-- Inside


insideView : ( Icon Msg, Label, Routing.Page ) -> Node
insideView ( icon, label, itemPage ) =
    itemPage
        |> Routing.GoToPage
        |> RoutingMsg
        |> (,,) icon label
        |> insideViewCustom


insideViewCustom : ( Icon Msg, Label, Msg ) -> Node
insideViewCustom ( Icon icon, Label label, msg ) =
    let
        maybeHref : Maybe String
        maybeHref =
            case msg of
                RoutingMsg (Routing.GoToPage page) ->
                    Just (pageToHref page)

                _ ->
                    Nothing

        attributes : List (Attribute variations Msg)
        attributes =
            [ onWithOptions "click" preventDefaultOptions (Json.Decode.succeed msg)

            -- Title
            , case label of
                Hidden l ->
                    attribute "title" l

                Shown _ ->
                    attribute "title" ""

            -- Width
            , case label of
                Hidden _ ->
                    width content

                Shown _ ->
                    width fill
            ]

        node : Node
        node =
            row
                Zed
                [ height (scaledPx 9)
                , center
                , moveDown 1
                , paddingXY (scaled 2) 0
                , verticalCenter
                , width fill

                -- Spacing
                , case label of
                    Hidden _ ->
                        spacing 0

                    Shown l ->
                        spacing (scaled -2)
                ]
                [ html (icon colorDerivatives.text 16)

                -- Label
                , case label of
                    Hidden _ ->
                        empty

                    Shown l ->
                        text l
                ]
    in
        case maybeHref of
            Just href ->
                el (Navigation InsideItem) attributes (link href node)

            Nothing ->
                el (Navigation InsideItem) attributes node
