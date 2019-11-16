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



-- 🍱  ░░  BUTTON


type ButtonColor
    = Accent
    | Gray
    | White


type ButtonType
    = Filled
    | IconOnly
    | Normal


button : ButtonType -> msg -> Html msg -> Html msg
button =
    buttonWithColor Accent


buttonLink : String -> ButtonType -> Html msg -> Html msg
buttonLink theHref buttonType =
    buttonWithOptions Html.a [ href theHref ] Accent buttonType Nothing


buttonLinkWithColor : ButtonColor -> String -> ButtonType -> Html msg -> Html msg
buttonLinkWithColor color theHref buttonType =
    buttonWithOptions Html.a [ href theHref ] color buttonType Nothing


buttonWithColor : ButtonColor -> ButtonType -> msg -> Html msg -> Html msg
buttonWithColor color buttonType msg =
    buttonWithOptions Html.button [] color buttonType (Just msg)


buttonWithOptions :
    (List (Html.Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Html.Attribute msg)
    -> ButtonColor
    -> ButtonType
    -> Maybe msg
    -> Html msg
    -> Html msg
buttonWithOptions tag attributes buttonColor buttonType maybeMsg child =
    let
        defaultClasses =
            [ C.antialiased
            , C.border_2
            , C.cursor_pointer
            , C.font_bold
            , C.inline_block
            , C.no_underline
            , C.py_2
            , C.px_4
            , C.rounded
            , C.text_center
            , C.text_sm

            --
            , C.fixate__bg_white
            , C.fixate__border_black
            , C.fixate__text_black
            ]

        specificClasses =
            case buttonType of
                Filled ->
                    case buttonColor of
                        Accent ->
                            [ C.bg_accent, C.border_transparent, C.text_white ]

                        Gray ->
                            [ C.bg_base04, C.border_transparent, C.text_white ]

                        White ->
                            [ C.bg_white, C.border_transparent, C.text_white ]

                _ ->
                    case buttonColor of
                        Accent ->
                            [ C.bg_transparent, C.border_accent, C.text_accent ]

                        Gray ->
                            [ C.bg_transparent, C.border_base04, C.text_base04 ]

                        White ->
                            [ C.bg_transparent, C.border_white, C.text_white ]
    in
    slab
        tag
        (case maybeMsg of
            Just msg ->
                [ onClick msg ]

            Nothing ->
                []
        )
        (List.append
            defaultClasses
            specificClasses
        )
        [ case buttonType of
            IconOnly ->
                slab
                    Html.span
                    [ style "font-size" "0" ]
                    [ C.inline_block, C.leading_none, C.align_top ]
                    [ child ]

            _ ->
                inline
                    [ C.inline_block, C.leading_normal, C.pt_px ]
                    [ child ]
        ]



-- 🍱  ░░  OTHER


canister : List (Html msg) -> Html msg
canister children =
    chunk
        [ C.mx_1, C.px_4, C.pb_4 ]
        children


canisterForm : List (Html msg) -> Html msg
canisterForm children =
    chunk
        [ C.mx_1, C.px_4, C.pb_4, C.w_full ]
        children


centeredContent : List (Html msg) -> Html msg
centeredContent children =
    chunk
        [ C.flex
        , C.flex_grow
        , C.overflow_hidden
        , C.relative
        ]
        [ -- TODO: Html.map never logoBackdrop
          chunk
            [ C.flex
            , C.flex_col
            , C.flex_grow
            , C.items_center
            , C.justify_center
            ]
            children
        ]


checkbox : { checked : Bool, toggleMsg : msg } -> Html msg
checkbox opts =
    brick
        [ onClick opts.toggleMsg
        , style "left" "-3px"
        ]
        [ C.inline_block, C.cursor_pointer, C.relative ]
        [ if opts.checked then
            Icons.check_box 22 Inherit

          else
            Icons.check_box_outline_blank 22 Inherit
        ]


h1 : String -> Html msg
h1 text =
    slab
        Html.h1
        [ style "font-size" "13.5px" ]
        [ C.all_small_caps
        , C.antialiased
        , C.bg_base06
        , C.inline_block
        , C.font_semibold
        , C.leading_tight
        , C.m_0
        , C.minus_top_px
        , C.overflow_hidden
        , C.pointer_events_none
        , C.px_2
        , C.py_1
        , C.relative
        , C.rounded_b
        , C.uppercase
        , C.text_sm
        , C.text_white
        ]
        [ Html.text text ]


h2 : String -> Html msg
h2 text =
    slab
        Html.h2
        []
        -- TODO: [ css headerFontStyles ]
        [ C.mx_auto
        , C.text_2xl
        , C.font_bold
        , C.leading_tight
        , C.mb_6
        , C.mt_3
        , C.text_center
        ]
        [ Html.text text ]


h3 : String -> Html msg
h3 text =
    slab
        Html.h2
        []
        -- TODO: [ css headerFontStyles ]
        [ C.text_lg
        , C.font_bold
        , C.leading_tight
        , C.mb_6
        ]
        [ Html.text text ]


inlineIcon : (Int -> Coloring -> Svg.Svg msg) -> Html msg
inlineIcon icon =
    inline
        [ C.align_sub
        , C.inline_block
        , C.leading_0
        , C.mr_1
        , C.text_0
        ]
        [ icon 14 Inherit ]


intro : Html msg -> Html msg
intro child =
    slab
        Html.p
        [ style "line-height" "1.75" ]
        [ C.my_3
        , C.text_base05
        , C.text_sm
        ]
        [ child ]


label : List (Html.Attribute msg) -> String -> Html msg
label attributes t =
    slab
        Html.label
        (style "font-size" "11.25px" :: attributes)
        [ C.antialiased
        , C.block
        , C.font_bold
        , C.opacity_90
        , C.uppercase
        ]
        [ Html.text t ]


link : { label : String, url : String } -> Html msg
link params =
    slab
        Html.a
        [ href params.url ]
        -- TODO: [ css linkStyles, href params.url ]
        [ C.no_underline, C.text_inherit ]
        [ Html.text params.label ]


logoBackdrop : Html Never
logoBackdrop =
    brick
        -- TODO: [ css logoBackdropStyles ]
        []
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
        -- TODO: [ css selectStyles.container ]
        []
        [ C.relative
        , C.w_full
        ]
        [ slab
            Html.select
            -- TODO: [ css selectStyles.field ]
            [ onInput inputHandler ]
            [ C.appearance_none
            , C.bg_transparent
            , C.block
            , C.text_lg
            , C.leading_normal
            , C.m_0
            , C.rounded_none
            , C.outline_none
            , C.py_2
            , C.px_0
            , C.w_full
            ]
            options
        , brick
            -- TODO: [ css selectStyles.arrow ]
            []
            [ C.absolute, C.right_0 ]
            [ Icons.keyboard_arrow_down 20 (Color colorKit.base05) ]
        ]


textArea : List (Html.Attribute msg) -> Html msg
textArea attributes =
    slab
        Html.textarea
        attributes
        -- TODO: (css textAreaStyles :: attributes)
        [ C.bg_white
        , C.block
        , C.text_sm
        , C.leading_normal
        , C.mb_3
        , C.p_3
        , C.rounded
        , C.w_full
        ]
        []


textButton : { label : String, onClick : msg } -> Html msg
textButton params =
    slab
        Html.button
        -- TODO: [ css linkStyles ]
        [ onClick params.onClick ]
        [ C.bg_transparent
        , C.border_none
        , C.text_inherit
        , C.leading_tight
        , C.m_0
        , C.p_0
        , C.cursor_pointer
        ]
        [ Html.text params.label ]


textField : List (Html.Attribute msg) -> Html msg
textField attributes =
    slab
        Html.input
        -- TODO: (css textFieldStyles :: attributes)
        attributes
        [ C.border_none
        , C.bg_transparent
        , C.block
        , C.leading_normal
        , C.mt_1
        , C.py_2
        , C.rounded_none
        , C.text_sm
        , C.w_full
        ]
        []


textFieldAlt : List (Html.Attribute msg) -> Html msg
textFieldAlt attributes =
    slab
        Html.input
        attributes
        -- TODO: (css textFieldAltStyles :: attributes)
        [ C.bg_white
        , C.block
        , C.leading_normal
        , C.mb_3
        , C.p_3
        , C.rounded
        , C.text_sm
        , C.w_full
        ]
        []



-----------------------------------------
-- ㊙️
-----------------------------------------
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
