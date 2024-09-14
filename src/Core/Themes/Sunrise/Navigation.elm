module Themes.Sunrise.Navigation exposing (global, local, localWithTabindex)

import Alfred exposing (Alfred)
import Chunky exposing (..)
import Common
import Conditional exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (href, style, tabindex, target, title)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import UI.Navigation exposing (..)
import UI.Page as Page exposing (Page)



-- GLOBAL


global : List ( Page, String ) -> Maybe (Alfred reply) -> Page -> Html msg
global items alfred activePage =
    brick
        [ style "font-size" "11.25px" ]
        [ "antialiased"
        , "font-semibold"
        , "mb-16"
        , "mt-8"
        , "text-xs"
        , "tracking-widest"
        , "uppercase"

        --
        , ifThenElse (Maybe.isJust alfred) "opacity-0" "opacity-100"
        ]
        (List.indexedMap
            (globalItem activePage <| List.length items)
            items
        )


globalItem : Page -> Int -> Int -> ( Page, String ) -> Html msg
globalItem activePage totalItems idx ( page, label ) =
    let
        isActivePage =
            Page.sameBase page activePage

        isLastItem =
            idx + 1 == totalItems
    in
    chunk
        [ "inline-block"
        , ifThenElse isLastItem "mr-0" "mr-1"
        ]
        [ slab
            Html.a
            [ href (Page.toString page) ]
            [ "inline-block"
            , "leading-normal"
            , "no-underline"
            , "cursor-pointer"
            , "pt-2"

            --
            , ifThenElse isActivePage "border-b" "border-b-0"
            , ifThenElse isActivePage "border-base01-15" "border-transparent"
            , ifThenElse isActivePage "text-base01" "text-base01-55"
            , ifThenElse isLastItem "mr-0" "mr-8"

            --
            , "focus:border-black-50"
            , "focus:outline-none"
            , "focus:text-black"
            ]
            [ text label ]
        ]



-- LOCAL


local : List ( Icon msg, Label, Action msg ) -> Html msg
local =
    localWithTabindex 0


localWithTabindex : Int -> List ( Icon msg, Label, Action msg ) -> Html msg
localWithTabindex tabindex_ items =
    brick
        [ style "font-size" "12.5px" ]
        [ "antialiased"
        , "border-b"
        , "border-gray-300"

        -- Dark mode
        ------------
        , "dark:border-base01"
        ]
        [ chunk
            [ "flex" ]
            (items
                |> List.reverse
                |> List.map (localItem tabindex_ { amount = List.length items })
                |> List.reverse
            )
        ]


localItem : Int -> { amount : Int } -> ( Icon msg, Label, Action msg ) -> Html msg
localItem tabindex_ { amount } ( Icon icon, Label labelText labelType, action ) =
    slab
        (case action of
            NavigateToPage _ ->
                Html.a

            OpenLinkInNewPage _ ->
                Html.a

            PerformMsg _ ->
                Html.button

            PerformMsgWithMouseEvent _ ->
                Html.button
        )
        [ case action of
            NavigateToPage page ->
                href (Page.toString page)

            OpenLinkInNewPage link ->
                href link

            PerformMsg msg ->
                onClick msg

            PerformMsgWithMouseEvent msg ->
                Mouse.onClick msg

        --
        , case labelType of
            Hidden ->
                title labelText

            Shown ->
                title ""

        --
        , case action of
            OpenLinkInNewPage _ ->
                target "_blank"

            _ ->
                target "_self"

        --
        , tabindex tabindex_
        ]
        [ "bg-transparent"
        , "border-gray-300"
        , "border-r"
        , "cursor-pointer"
        , "flex-basis-0"
        , "font-semibold"
        , "leading-none"
        , "no-underline"
        , "px-4"
        , "py-3"
        , "text-base02"

        --
        , ifThenElse
            (labelText == Common.backToIndex && labelType == Hidden && amount > 1)
            "flex-shrink-0"
            "flex-grow"

        --
        , ifThenElse
            (labelText == Common.backToIndex && labelType == Hidden && amount > 1)
            "overflow-visible"
            "overflow-hidden"

        --
        , "fixate:text-black"
        , "last:border-r-0"

        -- Responsive
        -------------
        , "sm:overflow-visible"

        --
        , ifThenElse
            (labelType == Hidden)
            "sm:flex-shrink-0"
            "sm:flex-grow"

        --
        , ifThenElse
            (labelType == Hidden)
            "sm:flex-grow-0"
            "sm:flex-grow"

        -- Dark mode
        ------------
        , "dark:border-base01"
        , "dark:text-base06"

        --
        , "dark:fixate:text-base07"
        ]
        [ chunk
            [ "border-b"
            , "border-t"
            , "border-transparent"
            , "flex"
            , "items-center"
            , "justify-center"
            , "mt-px"
            , "pt-px"
            ]
            [ inline
                [ "flex-shrink-0" ]
                [ icon 16 Inherit ]

            --
            , case labelType of
                Hidden ->
                    nothing

                Shown ->
                    slab
                        Html.span
                        []
                        [ "hidden"
                        , "leading-tight"
                        , "ml-1"
                        , "transform"
                        , "translate-y-px"
                        , "truncate"

                        -- Responsive
                        -------------
                        , "sm:inline-block"
                        ]
                        [ text labelText ]
            ]
        ]
