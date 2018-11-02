module UI.Navigation exposing (Action(..), Icon(..), Label(..), LabelType(..), global, local)

import Chunky exposing (..)
import Color exposing (Color)
import Conditional exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (href, style, title)
import Html.Events exposing (onClick)
import List.Extra as List
import String.Format
import Svg exposing (Svg)
import Tachyons.Classes as T
import UI.Core exposing (Msg(..))
import UI.Kit
import UI.Page as Page exposing (Page)



-- 🌳


type Icon msg
    = Icon (Color -> Int -> Svg msg)


type Label
    = Label String LabelType


type LabelType
    = Hidden
    | Shown


type Action
    = GoToPage Page
    | PerformMsg Msg



-- Global


global : List ( Page, String ) -> Page -> Html Msg
global items activePage =
    block
        [ style "font-size" "11.5px" ]
        [ T.f7
        , T.mb5
        , T.mt4
        , T.tracked
        , T.ttu
        ]
        (List.indexedMap (globalItem activePage <| List.length items) items)


globalItem : Page -> Int -> Int -> ( Page, String ) -> Html Msg
globalItem activePage totalItems idx ( page, label ) =
    let
        isActivePage =
            page == activePage

        isLastItem =
            idx + 1 == totalItems
    in
    slab
        Html.a
        [ href (Page.toString page)

        --
        , style "color" (ifThenElse isActivePage globalActiveColor globalDefaultColor)
        , style "border-bottom-color" (ifThenElse isActivePage globalBorderColor "transparent")
        ]
        [ T.bb
        , T.dib
        , T.lh_copy
        , T.no_underline
        , T.pt2

        --
        , ifThenElse isLastItem T.mr0 T.mr4
        ]
        [ text label ]


{-| TODO - Wait for avh4/elm-color v1.1.0
-}
globalActiveColor =
    "rgb(65, 50, 63)"


globalDefaultColor =
    "rgba(65, 50, 63, 0.725)"


globalBorderColor =
    "rgba(65, 50, 63, 0.125)"



-- Local


local : List ( Icon Msg, Label, Action ) -> Page -> Html Msg
local items activePage =
    block
        [ style "font-size" "12.5px"
        , style "border-bottom-color" localBorderColor
        ]
        [ T.bb, T.flex ]
        (items
            |> List.reverse
            |> List.indexedMap localItem
            |> List.reverse
        )


localItem : Int -> ( Icon Msg, Label, Action ) -> Html Msg
localItem idx ( Icon icon, Label labelText labelType, action ) =
    slab
        (case action of
            GoToPage _ ->
                Html.a

            PerformMsg _ ->
                Html.button
        )
        [ case action of
            GoToPage page ->
                href (Page.toString page)

            PerformMsg msg ->
                onClick msg

        --
        , case labelType of
            Hidden ->
                title labelText

            Shown ->
                title ""

        --
        , style "border-right-color" localBorderColor
        , style "border-right-style" "solid"
        , style "border-right-width" (ifThenElse (idx == 0) "0" "1px")
        , style "border-top" "1px solid transparent"
        , style "color" localTextColor
        , style "height" "43px"
        ]
        [ ifThenElse (labelType == Hidden) T.flex_shrink_0 T.flex_grow_1

        --
        , T.bn
        , T.fw6
        , T.inline_flex
        , T.items_center
        , T.justify_center
        , T.lh_solid
        , T.no_underline
        , T.outline_0
        , T.ph3
        ]
        [ icon UI.Kit.colors.text 16

        --
        , case labelType of
            Hidden ->
                empty

            Shown ->
                slab
                    Html.span
                    []
                    [ T.dib, T.ml1 ]
                    [ text labelText ]
        ]


localBorderColor : String
localBorderColor =
    Color.toCssString UI.Kit.colors.subtleBorder


localTextColor : String
localTextColor =
    Color.toCssString UI.Kit.colors.text
