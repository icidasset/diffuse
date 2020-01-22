module UI.Navigation exposing (Action(..), Icon(..), Label(..), LabelType(..), global, local, localWithTabindex)

import Alfred exposing (Alfred)
import Chunky exposing (..)
import Conditional exposing (..)
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (href, style, tabindex, target, title)
import Html.Events exposing (onClick)
import List.Extra as List
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Svg exposing (Svg)
import UI.Page as Page exposing (Page)



-- 🌳


type Action msg
    = NavigateToPage Page
    | OpenLinkInNewPage String
    | PerformMsg msg


type Icon msg
    = Icon (Int -> Coloring -> Svg msg)


type Label
    = Label String LabelType


type LabelType
    = Hidden
    | Shown



-- GLOBAL


global : List ( Page, String ) -> Maybe (Alfred reply) -> Page -> Html msg
global items alfred activePage =
    brick
        [ style "font-size" "11.25px" ]
        [ C.antialiased
        , C.font_semibold
        , C.mb_16
        , C.mt_8
        , C.text_xs
        , C.tracking_widest
        , C.uppercase

        --
        , ifThenElse (Maybe.isJust alfred) C.opacity_0 C.opacity_100
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
        [ C.inline_block
        , ifThenElse isLastItem C.mr_0 C.mr_1
        ]
        [ slab
            Html.a
            [ href (Page.toString page) ]
            [ C.inline_block
            , C.leading_normal
            , C.no_underline
            , C.cursor_pointer
            , C.pt_2

            --
            , ifThenElse isActivePage C.border_b C.border_b_0
            , ifThenElse isActivePage C.border_base01_15 C.border_transparent
            , ifThenElse isActivePage C.text_base01 C.text_base01_55
            , ifThenElse isLastItem C.mr_0 C.mr_8

            --
            , C.focus__border_black_50
            , C.focus__outline_none
            , C.focus__text_black
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
        [ C.antialiased
        , C.border_b
        , C.border_gray_300

        -- Dark mode
        ------------
        , C.dark__border_base01
        ]
        [ chunk
            [ C.flex ]
            (items
                |> List.reverse
                |> List.map (localItem tabindex_)
                |> List.reverse
            )
        ]


localItem : Int -> ( Icon msg, Label, Action msg ) -> Html msg
localItem tabindex_ ( Icon icon, Label labelText labelType, action ) =
    slab
        (case action of
            NavigateToPage page ->
                Html.a

            OpenLinkInNewPage _ ->
                Html.a

            PerformMsg msg ->
                Html.button
        )
        [ case action of
            NavigateToPage page ->
                href (Page.toString page)

            OpenLinkInNewPage link ->
                href link

            PerformMsg msg ->
                onClick msg

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
        [ ifThenElse (labelType == Hidden) C.flex_shrink_0 C.flex_grow
        , C.bg_transparent
        , C.border_gray_300
        , C.border_r
        , C.cursor_pointer
        , C.flex_basis_0
        , C.font_semibold
        , C.leading_none
        , C.no_underline
        , C.px_4
        , C.py_3
        , C.text_base02

        --
        , C.fixate__text_black
        , C.last__border_r_0

        -- Dark mode
        ------------
        , C.dark__border_base01
        , C.dark__text_base06

        --
        , C.dark__fixate__text_base07
        ]
        [ chunk
            [ C.border_b
            , C.border_t
            , C.border_transparent
            , C.flex
            , C.items_center
            , C.justify_center
            , C.mt_px
            , C.pt_px
            ]
            [ icon 16 Inherit

            --
            , case labelType of
                Hidden ->
                    nothing

                Shown ->
                    slab
                        Html.span
                        []
                        [ C.inline_block, C.leading_tight, C.ml_1, C.truncate ]
                        [ text labelText ]
            ]
        ]
