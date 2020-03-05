module UI.Kit exposing (..)

import Chunky exposing (..)
import Chunky.Styled
import Color
import Color.Ext as Color
import Color.Manipulate as Color
import Conditional exposing (ifThenElse)
import Css
import Css.Classes as C
import Html exposing (Html)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick, onInput)
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
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
    { -- States
      success = colorKit.base0B
    , error = colorKit.base08
    , warning = colorKit.base0A

    -- Other
    , background = rgb 2 7 14
    , selection = colorKit.accent
    , text = colorKit.base01
    }


rgb =
    Color.rgb255



-- 🍱  ░░  BUTTON


type ButtonColor
    = Accent
    | Blank
    | Gray


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
            , C.leading_relaxed
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
                            [ C.bg_accent
                            , C.border_transparent
                            , C.text_white
                            ]

                        Blank ->
                            [ C.bg_white
                            , C.border_transparent
                            , C.text_accent_light

                            -- Dark mode
                            ------------
                            , C.dark__bg_darkest_hour
                            , C.dark__text_accent_dark
                            ]

                        Gray ->
                            [ C.bg_base04
                            , C.border_transparent
                            , C.text_white

                            -- Dark mode
                            ------------
                            , C.dark__bg_base05
                            ]

                _ ->
                    case buttonColor of
                        Accent ->
                            [ C.bg_transparent
                            , C.border_accent
                            , C.text_accent
                            ]

                        Blank ->
                            [ C.bg_transparent
                            , C.border_white
                            , C.text_white
                            ]

                        Gray ->
                            [ C.bg_transparent
                            , C.border_base04
                            , C.text_base04

                            -- Dark mode
                            ------------
                            , C.dark__border_base05
                            , C.dark__text_base05
                            ]
    in
    slab
        tag
        (case maybeMsg of
            Just msg ->
                attributes ++ [ onClick msg ]

            Nothing ->
                attributes
        )
        (List.append
            defaultClasses
            specificClasses
        )
        [ case buttonType of
            IconOnly ->
                inline
                    [ C.align_middle
                    , C.inline_block
                    , C.leading_none
                    , C.pointer_events_none
                    , C.text_0
                    ]
                    [ child ]

            _ ->
                inline
                    [ C.align_middle
                    , C.inline_block
                    , C.leading_none
                    , C.pointer_events_none
                    ]
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
        [ logoBackdrop
            |> Html.Styled.map never
            |> Html.Styled.toUnstyled
        , chunk
            [ C.flex
            , C.flex_col
            , C.flex_grow
            , C.items_center
            , C.justify_center
            , C.relative
            , C.z_10
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

        -- Dark mode
        ------------
        , C.dark__bg_base01
        , C.dark__text_base05
        ]
        [ Html.text text ]


h2 : String -> Html msg
h2 text =
    slab
        Html.h2
        []
        [ C.antialiased
        , C.font_bold
        , C.font_display
        , C.leading_tight
        , C.mb_8
        , C.mt_4
        , C.mx_auto
        , C.text_2xl
        , C.text_center
        ]
        [ Html.text text ]


h3 : String -> Html msg
h3 text =
    slab
        Html.h2
        []
        [ C.antialiased
        , C.font_bold
        , C.font_display
        , C.leading_tight
        , C.mb_8
        , C.mt_4
        , C.text_xl
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
        [ C.mb_6
        , C.mt_3
        , C.text_base05
        , C.text_sm

        -- Dark mode
        ------------
        , C.dark__text_base03
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
        , C.leading_normal
        , C.opacity_90
        , C.uppercase
        ]
        [ Html.text t ]


link : { label : String, url : String } -> Html msg
link params =
    slab
        Html.a
        [ href params.url ]
        [ C.border_b_2
        , C.border_accent
        , C.inline_block
        , C.leading_none
        , C.no_underline
        , C.text_inherit
        ]
        [ Html.text params.label ]


logoBackdrop : Html.Styled.Html Never
logoBackdrop =
    Chunky.Styled.brick
        [ css logoBackdropStyles ]
        [ C.absolute
        , C.bg_cover
        , C.bg_no_repeat
        , C.duration_1000
        , C.h_0
        , C.left_full
        , C.opacity_025
        , C.pt_full
        , C.top_0
        , C.transition
        , C.transition_opacity
        , C.z_0

        -- Dark mode
        ------------
        , C.dark__opacity_40
        ]
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

        -- Dark mode
        ------------
        , C.dark__bg_darkest_hour

        --
        , ifThenElse scrolling C.overflow_y_auto C.overflow_y_hidden
        ]


select : (String -> msg) -> List (Html msg) -> Html msg
select inputHandler options =
    chunk
        [ C.max_w_md
        , C.mx_auto
        , C.relative
        , C.text_base05
        , C.w_full

        --
        , C.focus_within__text_black

        -- Dark mode
        ------------
        , C.dark__text_gray_600
        , C.dark__focus_within__text_base07
        ]
        [ slab
            Html.select
            [ onInput inputHandler ]
            [ C.appearance_none
            , C.border_b
            , C.border_l_0
            , C.border_r_0
            , C.border_t_0
            , C.border_gray_400
            , C.bg_transparent
            , C.block
            , C.leading_normal
            , C.m_0
            , C.outline_none
            , C.py_2
            , C.px_0
            , C.rounded_none
            , C.text_base01
            , C.text_lg
            , C.w_full

            --
            , C.focus__border_black

            -- Dark mode
            ------------
            , C.dark__border_base02
            , C.dark__text_gray_600

            --
            , C.dark__focus__border_base07
            ]
            options
        , chunk
            [ C.absolute
            , C.minus_translate_y_half
            , C.mt_px
            , C.right_0
            , C.text_0
            , C.top_half
            , C.transform
            ]
            [ Icons.keyboard_arrow_down 20 Inherit ]
        ]


textArea : List (Html.Attribute msg) -> Html msg
textArea attributes =
    slab
        Html.textarea
        attributes
        [ C.bg_white
        , C.block
        , C.leading_normal
        , C.mb_4
        , C.p_4
        , C.resize_none
        , C.rounded
        , C.text_base01
        , C.text_sm
        , C.w_full

        -- Dark mode
        ------------
        , C.dark__bg_darkest_hour
        , C.dark__text_gray_600
        ]
        []


textButton : { label : String, onClick : msg } -> Html msg
textButton params =
    slab
        Html.button
        [ onClick params.onClick ]
        [ C.appearance_none
        , C.bg_transparent
        , C.border_accent
        , C.border_b_2
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
        attributes
        [ C.appearance_none
        , C.border_b
        , C.border_l_0
        , C.border_r_0
        , C.border_t_0
        , C.border_gray_400
        , C.bg_transparent
        , C.block
        , C.leading_normal
        , C.mt_1
        , C.py_2
        , C.rounded_none
        , C.text_base01
        , C.text_sm
        , C.w_full

        --
        , C.focus__border_black
        , C.focus__outline_none

        -- Dark mode
        ------------
        , C.dark__border_base02
        , C.dark__text_gray_600

        --
        , C.dark__focus__border_base07
        ]
        []


textFieldAlt : List (Html.Attribute msg) -> Html msg
textFieldAlt attributes =
    slab
        Html.input
        attributes
        [ C.bg_white
        , C.block
        , C.leading_normal
        , C.mb_3
        , C.p_3
        , C.resize_none
        , C.rounded
        , C.text_base01
        , C.text_sm
        , C.w_full

        -- Dark mode
        ------------
        , C.dark__bg_darkest_hour
        , C.dark__text_gray_600
        ]
        []



-----------------------------------------
-- ㊙️
-----------------------------------------


logoBackdropStyles : List Css.Style
logoBackdropStyles =
    [ Css.backgroundImage (Css.url "images/diffuse__icon-dark.svg")
    , Css.backgroundPosition2 (Css.pct -43.5) (Css.px 98)
    , Css.property "transform-origin" "left top"
    , Css.transform (Css.rotate (Css.deg 90))
    , Css.width (Css.vh 105)
    ]
