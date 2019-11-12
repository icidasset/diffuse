module UI.Kit exposing (..)

import Chunky exposing (..)
import Color
import Color.Ext as Color
import Color.Manipulate as Color
import Conditional exposing (ifThenElse)
import Css.Classes as C
import Html exposing (Html)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick, onInput)
import Material.Icons exposing (Coloring(..))
import Material.Icons.Hardware as Icons
import Material.Icons.Toggle as Icons
import Svg
import Tachyons.Classes as T



-- COLORS


colorKit =
    { base00 = rgb 45 45 45
    , base01 = rgb 63 63 63
    , base02 = rgb 79 79 79
    , base03 = rgb 119 119 119
    , base04 = rgb 140 140 140
    , base05 = rgb 163 163 163
    , base06 = rgb 186 186 186
    , base07 = rgb 232 232 232
    , base08 = rgb 239 97 85
    , base09 = rgb 249 155 21
    , base0A = rgb 254 196 24
    , base0B = rgb 72 182 133
    , base0C = rgb 91 196 191
    , base0D = rgb 6 182 239
    , base0E = rgb 129 91 164
    , base0F = rgb 233 107 168

    -- ~(˘▾˘~)
    , accent = rgb 231 150 128
    }


colors =
    { errorBorder = colorKit.base08
    , inputBorder = rgb 225 225 225
    , subtleBorder = rgb 238 238 238
    , verySubtleBorder = rgb 248 248 248

    -- States
    , success = colorKit.base0B
    , error = colorKit.base08
    , warning = colorKit.base0A

    -- Other
    , background = rgb 2 7 14
    , focus = rgb 0 0 0
    , selection = colorKit.base08
    , selectionAlt = colorKit.base01
    , text = colorKit.base01
    }


rgb =
    Color.rgb255



-- FOCUSING
--
-- focusWhileNotActive : List Css.Style -> Css.Style
-- focusWhileNotActive styles =
--     Css.batch
--         [ Css.outline none
--         , Css.pseudoClass "focus:not(:active)" styles
--         ]
--
--
-- focus : List Css.Style -> Css.Style
-- focus styles =
--     Css.batch
--         [ Css.outline none
--         , Css.focus styles
--         ]
--
-- FOCUSING, Pt. II
--
-- buttonFocus : Css.Style
-- buttonFocus =
--     focusWhileNotActive
--         [ Css.backgroundColor (Css.rgb 255 255 255)
--         , Css.borderColor (Color.toElmCssColor colors.focus)
--         , Css.color (Color.toElmCssColor colors.focus)
--         , iconFocusStyle
--         ]
--
--
-- inputFocus : Css.Style
-- inputFocus =
--     focusWhileNotActive
--         [ Css.borderBottomColor (Color.toElmCssColor colors.focus) ]
--
--
-- navFocus : Css.Style
-- navFocus =
--     focusWhileNotActive
--         [ Css.borderTopColor (Color.toElmCssColor colors.focus)
--         , iconFocusStyle
--         ]
--
--
-- textFocus : Css.Style
-- textFocus =
--     focus
--         [ Css.borderBottomColor (Css.rgba 0 0 0 0.475)
--         , Css.color (Color.toElmCssColor colors.focus)
--         ]
--
--
-- textAreaFocus : Css.Style
-- textAreaFocus =
--     focus
--         [ Css.color (Color.toElmCssColor colors.focus)
--         ]
--
-- FONTS
--
-- defaultFontFamilies : List String
-- defaultFontFamilies =
--     [ "Source Sans Pro", "sans-serif" ]
--
-- defaultFontStyles : List Css.Style
-- defaultFontStyles =
--     [ Css.fontFamilies defaultFontFamilies ]
--
-- headerFontFamilies : List String
-- headerFontFamilies =
--     [ "Montserrat", "Futura", "\"Trebuchet MS\"", "Arial", "sans-serif" ]
--
-- headerFontStyles : List Css.Style
-- headerFontStyles =
--     [ Css.fontFamilies headerFontFamilies ]
--
-- SHADOWS
--
-- onOverlayShadow : Css.Style
-- onOverlayShadow =
--     Css.property
--         "box-shadow"
--         "0 1px 3px 0 rgba(0, 0, 0, 0.175), 0 3px 15px 0 rgba(0, 0, 0, 0.075)"
--
-- SPACE PROPERTIES
--
-- borderRadius : String
-- borderRadius =
--     T.br2


insulationWidth : Float
insulationWidth =
    107.5



-- NODES


type ButtonType
    = Filled
    | IconOnly
    | Normal


button : ButtonType -> msg -> Html msg -> Html msg
button =
    buttonWithColor colorKit.accent


buttonLink : String -> ButtonType -> Html msg -> Html msg
buttonLink theHref buttonType =
    buttonWithOptions Html.a [ href theHref ] colorKit.accent buttonType Nothing


buttonLinkWithColor : Color.Color -> String -> ButtonType -> Html msg -> Html msg
buttonLinkWithColor color theHref buttonType =
    buttonWithOptions Html.a [ href theHref ] color buttonType Nothing


buttonWithColor : Color.Color -> ButtonType -> msg -> Html msg -> Html msg
buttonWithColor color buttonType msg =
    buttonWithOptions Html.button [] color buttonType (Just msg)


buttonWithOptions :
    (List (Html.Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Html.Attribute msg)
    -> Color.Color
    -> ButtonType
    -> Maybe msg
    -> Html msg
    -> Html msg
buttonWithOptions tag attributes buttonColor buttonType maybeMsg child =
    slab
        tag
        (List.append
            attributes
            [ css (buttonStyles buttonType buttonColor)
            , case maybeMsg of
                Just msg ->
                    onClick msg

                Nothing ->
                    style "carry" "on"
            ]
        )
        [ C.bg_transparent
        , C.cursor_pointer
        , C.font_bold
        , C.inline_block
        , C.no_underline
        , C.px_4
        , C.rounded
        , C.text_center
        , C.text_sm
        ]
        [ case buttonType of
            IconOnly ->
                slab
                    Html.span
                    [ style "font-size" "0" ]
                    [ C.inline_block, C.leading_none, C.align_top ]
                    [ child ]

            _ ->
                inline
                    [ C.leading_normal ]
                    [ child ]
        ]


canister : List (Html msg) -> Html msg
canister children =
    chunk
        [ T.mh1, T.ph3, T.pb3 ]
        children


canisterForm : List (Html msg) -> Html msg
canisterForm children =
    chunk
        [ T.measure, T.mh1, T.ph3, T.pb3, T.w_100 ]
        children


centeredContent : List (Html msg) -> Html msg
centeredContent children =
    chunk
        [ T.flex
        , T.flex_grow_1
        , T.overflow_hidden
        , T.relative
        ]
        [ Html.map never logoBackdrop
        , chunk
            [ T.flex
            , T.flex_column
            , T.flex_grow_1
            , T.items_center
            , T.justify_center
            , T.relative
            , T.z_1
            ]
            children
        ]


checkbox : { checked : Bool, toggleMsg : msg } -> Html msg
checkbox opts =
    brick
        [ css checkboxStyles, onClick opts.toggleMsg ]
        [ T.dib, T.pointer, T.relative ]
        [ if opts.checked then
            Html.fromUnstyled (Icons.check_box 22 Inherit)

          else
            Html.fromUnstyled (Icons.check_box_outline_blank 22 Inherit)
        ]


h1 : String -> Html msg
h1 text =
    slab
        Html.h1
        [ css h1Styles ]
        [ borderRadius
        , T.br__bottom
        , T.dib
        , T.fw6
        , T.lh_title
        , T.ma0
        , T.ph2
        , T.pv1
        , T.relative
        , T.ttu
        , T.white
        ]
        [ Html.text text ]


h2 : String -> Html msg
h2 text =
    slab
        Html.h2
        [ css headerFontStyles ]
        [ T.center
        , T.f3
        , T.fw7
        , T.lh_title
        , T.mb4
        , T.mt3
        , T.tc
        ]
        [ Html.text text ]


h3 : String -> Html msg
h3 text =
    slab
        Html.h2
        [ css headerFontStyles ]
        [ T.f4
        , T.fw7
        , T.lh_title
        , T.mb4
        ]
        [ Html.text text ]


inlineIcon : (Int -> Coloring -> Svg.Svg msg) -> Html msg
inlineIcon icon =
    slab
        Html.span
        [ css inlineIconStyles ]
        [ T.dib, T.mr1 ]
        [ Html.fromUnstyled (icon 14 Inherit) ]


intro : Html msg -> Html msg
intro child =
    slab
        Html.p
        [ css introStyles ]
        [ T.f6
        , T.mv3
        , T.pv1
        ]
        [ child ]


label : List (Html.Attribute msg) -> String -> Html msg
label attributes t =
    slab
        Html.label
        (css labelStyles :: attributes)
        [ T.db
        , T.fw7
        , T.o_90
        , T.ttu
        ]
        [ Html.text t ]


link : { label : String, url : String } -> Html msg
link params =
    slab
        Html.a
        [ css linkStyles, href params.url ]
        [ T.color_inherit, C.no_underline ]
        [ Html.text params.label ]


logoBackdrop : Html Never
logoBackdrop =
    brick
        [ css logoBackdropStyles ]
        [ C.absolute, C.top_0, C.z_0 ]
        []


receptacle : { scrolling : Bool } -> List (Html msg) -> Html msg
receptacle { scrolling } =
    chunk
        [ C.absolute
        , C.bg_white
        , C.flex
        , C.flex_col
        , C.inset_0
        , C.overflow_x_hidden
        , C.scrolling_touch
        , C.z_50

        --
        , ifThenElse scrolling C.overflow_y_auto C.overflow_y_hidden
        ]


select : (String -> msg) -> List (Html msg) -> Html msg
select inputHandler options =
    brick
        [ css selectStyles.container ]
        [ T.relative
        , T.w_100
        ]
        [ slab
            Html.select
            [ css selectStyles.field, onInput inputHandler ]
            [ T.bn
            , T.bg_transparent
            , T.br0
            , T.db
            , T.f5
            , T.input_reset
            , T.lh_copy
            , T.ma0
            , T.outline_0
            , T.pv2
            , T.ph0
            , T.w_100
            ]
            options
        , brick
            [ css selectStyles.arrow ]
            [ T.absolute, T.right_0 ]
            [ Html.fromUnstyled <| Icons.keyboard_arrow_down 20 (Color colorKit.base05) ]
        ]


textArea : List (Html.Attribute msg) -> Html msg
textArea attributes =
    slab
        Html.textarea
        (css textAreaStyles :: attributes)
        [ T.bn
        , T.bg_white
        , T.br2
        , T.db
        , T.f6
        , T.lh_copy
        , T.mb3
        , T.pa3
        , T.w_100
        ]
        []


textButton : { label : String, onClick : msg } -> Html msg
textButton params =
    slab
        Html.button
        [ css linkStyles, onClick params.onClick ]
        [ T.bg_transparent, T.color_inherit, T.bn, T.lh_title, T.ma0, T.pa0, T.pointer ]
        [ Html.text params.label ]


textField : List (Html.Attribute msg) -> Html msg
textField attributes =
    slab
        Html.input
        (css textFieldStyles :: attributes)
        [ T.bn
        , T.bg_transparent
        , T.br0
        , T.db
        , T.f6
        , T.lh_copy
        , T.mt1
        , T.pv2
        , T.w_100
        ]
        []


textFieldAlt : List (Html.Attribute msg) -> Html msg
textFieldAlt attributes =
    slab
        Html.input
        (css textFieldAltStyles :: attributes)
        [ T.bn
        , T.bg_white
        , T.br2
        , T.db
        , T.f6
        , T.lh_copy
        , T.mb3
        , T.pa3
        , T.w_100
        ]
        []



-----------------------------------------
-- ㊙️
-----------------------------------------
--
--
-- buttonStyles : ButtonType -> Color.Color -> List Css.Style
-- buttonStyles buttonType buttonColor =
--     case buttonType of
--         Filled ->
--             [ Css.backgroundColor (Color.toElmCssColor buttonColor)
--             , Css.borderColor Css.transparent
--             , Css.color (Css.rgb 255 255 255)
--             , Css.paddingBottom (Css.rem 0.525)
--             , Css.paddingTop (Css.rem 0.6)
--             , buttonFocus
--             ]
--
--         _ ->
--             [ Css.borderColor (Color.toElmCssColor buttonColor)
--             , Css.color (Color.toElmCssColor buttonColor)
--             , Css.paddingBottom (Css.rem 0.525)
--             , Css.paddingTop (Css.rem 0.6)
--             , buttonFocus
--             ]
--
--
-- checkboxStyles : List Css.Style
-- checkboxStyles =
--     [ Css.left (Css.px -3)
--     ]
--
--
-- h1Styles : List Css.Style
-- h1Styles =
--     [ Css.backgroundColor (Color.toElmCssColor colorKit.base06)
--     , Css.fontSize (px 13.5)
--     , Css.fontVariant Css.allSmallCaps
--     , Css.pointerEvents Css.none
--     , Css.top (px -1)
--     ]
--
--
-- inlineIconStyles : List Css.Style
-- inlineIconStyles =
--     [ Css.fontSize (px 0)
--     , Css.lineHeight (px 0)
--     , Css.verticalAlign Css.sub
--
--     --
--     , Css.Global.descendants
--         [ Css.Global.selector "svg > g"
--             [ Css.fill Css.currentColor ]
--         ]
--     ]
--
--
-- introStyles : List Css.Style
-- introStyles =
--     [ Css.color (Color.toElmCssColor colorKit.base05)
--     , Css.lineHeight (em 1.75)
--     ]
--
--
-- labelStyles : List Css.Style
-- labelStyles =
--     [ Css.fontSize (px 11.25) ]
--
--
-- linkStyles : List Css.Style
-- linkStyles =
--     [ Css.borderBottom3 (px 2) solid (Color.toElmCssColor colorKit.accent) ]
--
--
-- logoBackdropStyles : List Css.Style
-- logoBackdropStyles =
--     [ Css.backgroundImage (url "images/diffuse__icon-dark.svg")
--     , Css.backgroundPosition2 (pct -43.5) (px 98)
--     , Css.backgroundRepeat Css.noRepeat
--     , Css.backgroundSize Css.cover
--     , Css.height zero
--     , Css.left (pct 100)
--     , Css.opacity (num 0.025)
--     , Css.paddingTop (pct 100)
--     , Css.property "transform-origin" "left top"
--     , Css.transform (Css.rotate (deg 90))
--     , Css.width (Css.vh 105)
--     ]
--
--
-- selectStyles : { arrow : List Css.Style, container : List Css.Style, field : List Css.Style }
-- selectStyles =
--     { arrow =
--         [ Css.fontSize (px 0)
--         , Css.marginTop (px 1)
--         , Css.top (pct 50)
--         , Css.transform (Css.translateY <| pct -50)
--         ]
--     , container =
--         []
--     , field =
--         [ Css.borderBottom3 (px 1) solid (Color.toElmCssColor colors.inputBorder)
--         , Css.color (Color.toElmCssColor colors.text)
--         , inputFocus
--
--         --
--         , Css.pseudoClass
--             "-moz-focusring"
--             [ Css.color Css.transparent
--             , Css.textShadow4 zero zero zero (Css.rgb 0 0 0)
--             ]
--         ]
--     }
--
--
-- textAreaStyles : List Css.Style
-- textAreaStyles =
--     [ Css.color (Color.toElmCssColor colors.text)
--     , Css.height (px 109)
--     , Css.maxWidth (Css.vw 87.5)
--     , Css.resize Css.none
--     , Css.width (px 292)
--     , textAreaFocus
--     ]
--
--
-- textFieldStyles : List Css.Style
-- textFieldStyles =
--     [ Css.borderBottom3 (px 1) solid (Color.toElmCssColor colors.inputBorder)
--     , Css.color (Color.toElmCssColor colors.text)
--     , inputFocus
--
--     --
--     , Css.invalid
--         [ Css.boxShadow none
--         , Css.outline none
--         ]
--
--     --
--     , (Css.focus << List.singleton << Css.invalid)
--         [ Css.borderBottomColor (Color.toElmCssColor colors.error) ]
--     ]
--
--
-- textFieldAltStyles : List Css.Style
-- textFieldAltStyles =
--     [ Css.color (Color.toElmCssColor colors.text)
--     , Css.maxWidth (Css.vw 87.5)
--     , Css.resize Css.none
--     , Css.width (px 292)
--     , textAreaFocus
--     ]
--
--
-- ⚗️
--
-- iconFocusStyle : Css.Style
-- iconFocusStyle =
--     [ Css.fill (Color.toElmCssColor colors.focus) ]
--         |> Css.Global.selector "svg > g"
--         |> List.singleton
--         |> Css.Global.descendants
