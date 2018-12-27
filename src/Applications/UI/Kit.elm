module UI.Kit exposing (ButtonType(..), button, buttonFocus, canister, centeredContent, colorKit, colors, defaultFontFamilies, h1, h2, h3, headerFontFamilies, inputFocus, insulationWidth, intro, label, link, logoBackdrop, navFocus, select, textField, textFocus, vessel)

import Chunky exposing (..)
import Color
import Color.Ext as Color
import Css exposing (deg, em, none, num, pct, px, solid, url, zero)
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, href, style)
import Html.Styled.Events exposing (onClick, onInput)
import Material.Icons.Hardware as Icons
import Tachyons.Classes as T



-- COLORS


colorKit =
    { base00 = rgb 47 30 46
    , base01 = rgb 65 50 63
    , base02 = rgb 79 66 76
    , base03 = rgb 119 110 113
    , base04 = rgb 141 134 135
    , base05 = rgb 163 158 155
    , base06 = rgb 185 182 176
    , base07 = rgb 231 233 219
    , base08 = rgb 239 97 85
    , base09 = rgb 249 155 21
    , base0A = rgb 254 196 24
    , base0B = rgb 72 182 133
    , base0C = rgb 91 196 191
    , base0D = rgb 6 182 239
    , base0E = rgb 129 91 164
    , base0F = rgb 233 107 168

    -- ~(˘▾˘~)
    , accent = rgb 248 164 167
    }


colors =
    { errorBorder = colorKit.base08
    , inputBorder = rgb 225 225 225
    , subtleBorder = rgb 238 238 238

    -- States
    , success = colorKit.base0B
    , error = colorKit.base08

    -- Other
    , background = rgb 2 7 14
    , focus = rgb 0 0 0
    , text = colorKit.base01
    }


rgb =
    Color.rgb255



-- FOCUSING


focusWhileNotActive : List Css.Style -> Css.Style
focusWhileNotActive styles =
    Css.batch
        [ Css.outline none
        , Css.pseudoClass "focus:not(:active)" styles
        ]


focus : List Css.Style -> Css.Style
focus styles =
    Css.batch
        [ Css.outline none
        , Css.focus styles
        ]



-- FOCUSING, Pt. II


buttonFocus : Css.Style
buttonFocus =
    focusWhileNotActive
        [ Css.borderColor (Color.toElmCssColor colors.focus)
        , Css.color (Color.toElmCssColor colors.focus)
        , iconFocusStyle
        ]


inputFocus : Css.Style
inputFocus =
    focusWhileNotActive
        [ Css.borderBottomColor (Color.toElmCssColor colors.focus) ]


navFocus : Css.Style
navFocus =
    focusWhileNotActive
        [ Css.borderTopColor (Color.toElmCssColor colors.focus)
        , iconFocusStyle
        ]


textFocus : Css.Style
textFocus =
    focus
        [ Css.borderBottomColor (Css.rgba 0 0 0 0.475)
        , Css.color (Color.toElmCssColor colors.focus)
        ]



-- FONTS


defaultFontFamilies : List String
defaultFontFamilies =
    [ "Source Sans Pro", "sans-serif" ]


defaultFontStyles : List Css.Style
defaultFontStyles =
    [ Css.fontFamilies headerFontFamilies ]


headerFontFamilies : List String
headerFontFamilies =
    [ "Montserrat", "Futura", "\"Trebuchet MS\"", "Arial", "sans-serif" ]


headerFontStyles : List Css.Style
headerFontStyles =
    [ Css.fontFamilies headerFontFamilies ]



-- SPACE PROPERTIES


borderRadius : String
borderRadius =
    T.br2


insulationWidth : Float
insulationWidth =
    840


maxInputWidth : Float
maxInputWidth =
    360



-- NODES


type ButtonType
    = WithIcon
    | WithText


button : ButtonType -> msg -> Html msg -> Html msg
button buttonType msg child =
    slab
        Html.button
        [ css buttonStyles
        , onClick msg
        ]
        [ borderRadius
        , T.b__solid
        , T.bg_transparent
        , T.bw1
        , T.f6
        , T.fw7
        , T.ph3
        , T.pointer
        , T.pv2
        ]
        [ case buttonType of
            WithIcon ->
                slab
                    Html.span
                    [ style "font-size" "0" ]
                    [ T.dib, T.lh_solid, T.v_top ]
                    [ child ]

            WithText ->
                chunk
                    [ T.lh_copy ]
                    [ child ]
        ]


canister : List (Html msg) -> Html msg
canister =
    chunk
        [ T.mh1, T.ph3 ]


centeredContent : List (Html msg) -> Html msg
centeredContent children =
    chunk
        [ T.flex
        , T.flex_grow_1
        , T.overflow_hidden
        , T.relative
        ]
        [ logoBackdrop
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


h1 : String -> Html msg
h1 text =
    slab
        Html.h1
        [ css h1Styles ]
        [ borderRadius
        , T.br__bottom
        , T.dt
        , T.fw7
        , T.lh_copy
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
        [ T.f3
        , T.fw7
        , T.lh_title
        , T.mb4
        , T.mt0
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
        [ T.color_inherit, T.no_underline ]
        [ Html.text params.label ]


logoBackdrop : Html msg
logoBackdrop =
    brick
        [ css logoBackdropStyles ]
        [ T.absolute, T.top_0, T.z_0 ]
        []


select : (String -> msg) -> List (Html msg) -> Html msg
select inputHandler options =
    brick
        [ css selectStyles.container ]
        [ T.center
        , T.mb4
        , T.relative
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
            [ Html.fromUnstyled (Icons.keyboard_arrow_down colorKit.base05 20) ]
        ]


textField : List (Html.Attribute msg) -> Html msg
textField attributes =
    slab
        Html.input
        (css textFieldStyles :: attributes)
        [ T.bn
        , T.bg_transparent
        , T.db
        , T.f6
        , T.lh_copy
        , T.mt1
        , T.pv2
        , T.w_100
        ]
        []


vessel : List (Html msg) -> Html msg
vessel =
    brick
        [ css vesselStyles ]
        [ borderRadius
        , T.bg_white
        , T.flex
        , T.flex_column
        , T.flex_grow_1
        , T.overflow_hidden
        , T.w_100
        ]



-----------------------------------------
-- ㊙️
-----------------------------------------


buttonStyles : List Css.Style
buttonStyles =
    [ Css.borderColor (Color.toElmCssColor colorKit.accent)
    , Css.color (Color.toElmCssColor colorKit.accent)
    , buttonFocus
    ]


h1Styles : List Css.Style
h1Styles =
    [ Css.backgroundColor (Color.toElmCssColor colorKit.base06)
    , Css.fontSize (px 11.25)
    , Css.letterSpacing (px 0.25)
    , Css.top (px -1)
    ]


introStyles : List Css.Style
introStyles =
    [ Css.color (Color.toElmCssColor colorKit.base05)
    , Css.lineHeight (em 1.75)
    ]


labelStyles : List Css.Style
labelStyles =
    [ Css.fontSize (px 11.25) ]


linkStyles : List Css.Style
linkStyles =
    [ Css.borderBottom3 (px 2) solid (Color.toElmCssColor colorKit.accent) ]


logoBackdropStyles : List Css.Style
logoBackdropStyles =
    [ Css.backgroundImage (url "/images/diffuse__icon-dark.svg")
    , Css.backgroundPosition2 (pct -43.5) (px 98)
    , Css.backgroundRepeat Css.noRepeat
    , Css.backgroundSize Css.cover
    , Css.height zero
    , Css.left (pct 100)
    , Css.opacity (num 0.025)
    , Css.paddingTop (pct 100)
    , Css.property "transform-origin" "left top"
    , Css.transform (Css.rotate (deg 90))
    , Css.width (Css.vh 105)
    ]


selectStyles : { arrow : List Css.Style, container : List Css.Style, field : List Css.Style }
selectStyles =
    { arrow =
        [ Css.fontSize (px 0)
        , Css.marginTop (px 1)
        , Css.top (pct 50)
        , Css.transform (Css.translateY <| pct -50)
        ]
    , container =
        [ Css.maxWidth (px maxInputWidth) ]
    , field =
        [ Css.borderBottom3 (px 1) solid (Color.toElmCssColor colors.inputBorder)
        , Css.color (Color.toElmCssColor colors.text)
        , inputFocus
        ]
    }


textFieldStyles : List Css.Style
textFieldStyles =
    [ Css.borderBottom3 (px 1) solid (Color.toElmCssColor colors.inputBorder)
    , Css.color (Color.toElmCssColor colors.text)
    , Css.maxWidth (px maxInputWidth)
    , inputFocus
    ]


vesselStyles : List Css.Style
vesselStyles =
    [ Css.maxWidth (px insulationWidth) ]



-- ⚗️


iconFocusStyle : Css.Style
iconFocusStyle =
    [ Css.fill (Color.toElmCssColor colors.focus) ]
        |> Css.Global.selector "svg > g"
        |> List.singleton
        |> Css.Global.descendants
