module UI.Navigation exposing (Action(..), Icon(..), Label(..), LabelType(..), global, local)

import Chunky exposing (..)
import Color exposing (Color)
import Color.Ext as Color
import Color.Manipulate
import Conditional exposing (..)
import Css exposing (px, solid, transparent, zero)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes exposing (attribute, css, href, style, title)
import Html.Styled.Events exposing (onClick)
import List.Extra as List
import String.Format
import Svg exposing (Svg)
import Tachyons.Classes as T
import UI.Kit
import UI.Page as Page exposing (Page)



-- 🌳


type Action msg
    = GoToPage Page
    | PerformMsg msg


type Icon msg
    = Icon (Color -> Int -> Svg msg)


type Label
    = Label String LabelType


type LabelType
    = Hidden
    | Shown



-- GLOBAL


global : List ( Page, String ) -> Page -> Html msg
global items activePage =
    brick
        [ css globalStyles ]
        [ T.f7
        , T.mb5
        , T.mt4
        , T.tracked
        , T.ttu
        ]
        (List.indexedMap (globalItem activePage <| List.length items) items)


globalItem : Page -> Int -> Int -> ( Page, String ) -> Html msg
globalItem activePage totalItems idx ( page, label ) =
    let
        isActivePage =
            Page.sameBase page activePage

        isLastItem =
            idx + 1 == totalItems
    in
    slab
        Html.a
        [ attribute "data-keep-focus" "t"
        , css (globalItemStyles isActivePage)
        , href (Page.toString page)
        ]
        [ T.dib
        , T.lh_copy
        , T.no_underline
        , T.pointer
        , T.pt2

        --
        , ifThenElse isActivePage T.bb T.bn
        , ifThenElse isLastItem T.mr0 T.mr4
        ]
        [ text label ]


globalColors : { active : Css.Color, border : Css.Color, default : Css.Color }
globalColors =
    { active = Color.toElmCssColor UI.Kit.colorKit.base01
    , border = Color.toElmCssColor (Color.Manipulate.fadeOut 0.875 UI.Kit.colorKit.base01)
    , default = Color.toElmCssColor (Color.Manipulate.fadeOut 0.275 UI.Kit.colorKit.base01)
    }


globalStyles : List Css.Style
globalStyles =
    [ Css.fontSize (px 11.5) ]


globalItemStyles : Bool -> List Css.Style
globalItemStyles isActivePage =
    [ Css.borderBottomColor (ifThenElse isActivePage globalColors.border <| Css.rgba 0 0 0 0)
    , Css.color (ifThenElse isActivePage globalColors.active globalColors.default)
    , UI.Kit.textFocus
    ]



-- LOCAL


local : List ( Icon msg, Label, Action msg ) -> Html msg
local items =
    brick
        [ css localStyles ]
        [ T.bb, T.flex ]
        (items
            |> List.reverse
            |> List.map localItem
            |> List.reverse
        )


localItem : ( Icon msg, Label, Action msg ) -> Html msg
localItem ( Icon icon, Label labelText labelType, action ) =
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
        , css localItemStyles
        ]
        [ ifThenElse (labelType == Hidden) T.flex_shrink_0 T.flex_grow_1
        , T.bg_transparent
        , T.bl_0
        , T.fw6
        , T.inline_flex
        , T.items_center
        , T.justify_center
        , T.lh_solid
        , T.no_underline
        , T.pointer
        , T.ph3
        ]
        [ Html.fromUnstyled (icon UI.Kit.colors.text 16)

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


localColors : { border : Color, text : Color }
localColors =
    { border = UI.Kit.colors.subtleBorder
    , text = UI.Kit.colors.text
    }


localStyles : List Css.Style
localStyles =
    [ Css.borderBottomColor (Color.toElmCssColor localColors.border)
    , Css.fontSize (px 12.5)
    ]


localItemStyles : List Css.Style
localItemStyles =
    [ Css.borderBottom3 (px 1) solid transparent
    , Css.borderRight3 (px 1) solid (Color.toElmCssColor localColors.border)
    , Css.borderTop3 (px 2) solid transparent
    , Css.color (Color.toElmCssColor localColors.text)
    , Css.flexBasis (px 0)
    , Css.height (px 43)
    , UI.Kit.navFocus

    -- Last one
    -----------
    , Css.lastChild [ Css.borderRightWidth zero ]
    ]
