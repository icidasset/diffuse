module UI.Navigation exposing (Action(..), Icon(..), Label(..), LabelType(..), global, local, localWithTabindex)

import Alfred exposing (Alfred)
import Chunky exposing (..)
import Color exposing (Color)
import Color.Ext as Color
import Color.Manipulate
import Conditional exposing (..)
import Css exposing (px, solid, transparent, zero)
import Css.Media
import Html exposing (Html, text)
import Html.Attributes exposing (attribute, href, tabindex, target, title)
import Html.Events exposing (onClick)
import List.Extra as List
import Material.Icons exposing (Coloring(..))
import Maybe.Extra as Maybe
import Svg exposing (Svg)
import Tachyons.Classes as T
import UI.Css
import UI.Kit
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
        -- TODO: [ css globalStyles ]
        []
        [ T.f7
        , T.fw6
        , T.mb5
        , T.mt4
        , T.tracked
        , T.ttu

        --
        , ifThenElse (Maybe.isJust alfred) T.o_0 T.o_100
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
    chunk
        [ T.dib
        , ifThenElse isLastItem T.mr0 T.mr1
        ]
        [ slab
            Html.a
            [ attribute "data-keep-focus" "t"

            --, TODO: css (globalItemStyles isActivePage)
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
        ]


globalColors : { active : Css.Color, border : Css.Color, default : Css.Color }
globalColors =
    { active = Color.toElmCssColor UI.Kit.colorKit.base01
    , border = Color.toElmCssColor (Color.Manipulate.fadeOut 0.875 UI.Kit.colorKit.base01)
    , default = Color.toElmCssColor (Color.Manipulate.fadeOut 0.45 UI.Kit.colorKit.base01)
    }


globalStyles : List Css.Style
globalStyles =
    [ Css.fontSize (px 11.25) ]


globalItemStyles : Bool -> List Css.Style
globalItemStyles isActivePage =
    [ Css.borderBottomColor (ifThenElse isActivePage globalColors.border <| Css.rgba 0 0 0 0)
    , Css.color (ifThenElse isActivePage globalColors.active globalColors.default)

    --
    -- TODO: , UI.Kit.textFocus
    ]



-- LOCAL


local : List ( Icon msg, Label, Action msg ) -> Html msg
local =
    localWithTabindex 0


localWithTabindex : Int -> List ( Icon msg, Label, Action msg ) -> Html msg
localWithTabindex tabindex_ items =
    brick
        -- TODO: [ css localStyles ]
        []
        [ T.bb ]
        [ chunk
            [ T.flex ]
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
        -- TODO: , css localItemStyles
        , tabindex tabindex_
        ]
        [ ifThenElse (labelType == Hidden) T.flex_shrink_0 T.flex_grow_1
        , T.bg_transparent
        , T.bl_0
        , T.fw6
        , T.flex
        , T.items_center
        , T.justify_center
        , T.lh_solid
        , T.no_underline
        , T.pointer
        , T.ph3
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
                    [ T.dib, T.ml1, T.pv2, T.truncate ]
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
    , Css.fontSize (px 12)

    --
    , Css.Media.withMedia
        [ UI.Css.notSmallMediaQuery ]
        [ Css.fontSize (px 12.5) ]
    ]


localItemStyles : List Css.Style
localItemStyles =
    [ Css.borderBottom3 (px 1) solid transparent
    , Css.borderRight3 (px 1) solid (Color.toElmCssColor localColors.border)
    , Css.borderTop3 (px 2) solid transparent
    , Css.color (Color.toElmCssColor localColors.text)
    , Css.flexBasis (px 0)
    , Css.height (px 43)

    -- TODO: , UI.Kit.navFocus
    -- Last one
    -----------
    , Css.lastChild [ Css.borderRightWidth zero ]
    ]
