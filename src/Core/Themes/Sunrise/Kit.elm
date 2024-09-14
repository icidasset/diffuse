module Themes.Sunrise.Kit exposing (..)

import Chunky exposing (..)
import Color
import Conditional exposing (ifThenElse)
import Html exposing (Html)
import Html.Attributes as A exposing (href, style)
import Html.Events exposing (onClick, onInput)
import Icons exposing (Icon)
import Material.Icons.Round as Icons
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
    }


colors =
    { -- States
      success = colorKit.base0B
    , error = colorKit.base08
    , warning = colorKit.base0A

    -- Gray
    , gray_100 = Color.hsl 0 0 0.988
    , gray_200 = Color.hsl 0 0 0.973
    , gray_300 = Color.hsl 0 0 0.933
    , gray_400 = Color.hsl 0 0 0.882
    , gray_500 = Color.hsl 0 0 0.863
    , gray_600 = Color.hsl 0 0 0.776

    -- Other
    , background = rgb 2 7 14
    , selection = colorKit.base04
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
    buttonWithColor Gray


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
            [ "antialiased"
            , "border-2"
            , "cursor-pointer"
            , "font-semibold"
            , "inline-block"
            , "leading-relaxed"
            , "no-underline"
            , "py-2"
            , "px-4"
            , "rounded"
            , "text-center"
            , "text-sm"

            --
            , "fixate:bg-white"
            , "fixate:border-black"
            , "fixate:text-black"
            ]

        specificClasses =
            case buttonType of
                Filled ->
                    case buttonColor of
                        Accent ->
                            [ "bg-accent-btn"
                            , "border-transparent"
                            , "text-white-90"
                            ]

                        Blank ->
                            [ "bg-white"
                            , "border-transparent"
                            , "text-accent-light"

                            -- Dark mode
                            ------------
                            , "dark:bg-darkest-hour"
                            , "dark:text-accent-dark"
                            ]

                        Gray ->
                            [ "bg-base04"
                            , "border-transparent"
                            , "text-white"

                            -- Dark mode
                            ------------
                            , "dark:bg-base05"
                            ]

                _ ->
                    case buttonColor of
                        Accent ->
                            [ "bg-transparent"
                            , "border-accent-btn"
                            , "text-accent-btn"
                            ]

                        Blank ->
                            [ "bg-transparent"
                            , "border-white"
                            , "text-white"
                            ]

                        Gray ->
                            [ "bg-transparent"
                            , "border-base04"
                            , "text-base04"

                            -- Dark mode
                            ------------
                            , "dark:border-base05"
                            , "dark:text-base05"
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
                    [ "align-middle"
                    , "inline-block"
                    , "leading-none"
                    , "pointer-events-none"
                    , "text-0"
                    ]
                    [ child ]

            _ ->
                inline
                    [ "align-middle"
                    , "inline-block"
                    , "leading-none"
                    , "pointer-events-none"
                    ]
                    [ child ]
        ]



-- 🍱  ░░  OTHER


askForInput : { question : String, info : List (Html msg) } -> Html msg
askForInput { question, info } =
    Html.span
        []
        [ chunk
            [ "font-semibold", "pt-1" ]
            [ Html.text question ]
        , case info of
            [] ->
                Html.text ""

            _ ->
                chunk
                    [ "italic", "mt-2", "text-sm" ]
                    info
        ]


canister : List (Html msg) -> Html msg
canister children =
    chunk
        [ "mx-1", "px-4", "pb-4" ]
        children


canisterForm : List (Html msg) -> Html msg
canisterForm children =
    chunk
        [ "mx-1", "px-4", "pb-4", "w-full" ]
        children


centeredContent : List (Html msg) -> Html msg
centeredContent children =
    chunk
        [ "flex"
        , "flex-grow"
        , "items-stretch"
        , "overflow-hidden"
        , "relative"
        ]
        [ Html.map never logoBackdrop
        , chunk
            [ "flex"
            , "flex-col"
            , "flex-grow"
            , "items-center"
            , "justify-center"
            , "max-w-full"
            , "relative"
            , "z-10"
            ]
            children
        ]


checkbox : { checked : Bool, toggleMsg : msg } -> Html msg
checkbox opts =
    brick
        [ onClick opts.toggleMsg
        , style "left" "-3px"
        ]
        [ "inline-block", "cursor-pointer", "relative" ]
        [ if opts.checked then
            Icons.check_box 22 Inherit

          else
            Icons.check_box_outline_blank 22 Inherit
        ]


focusScreen : { icon : Icon msg, iconHref : Maybe String, text : List (Html msg), textHref : Maybe String } -> List (Html msg) -> Html msg
focusScreen { icon, iconHref, text, textHref } nodes =
    [ slab
        (case iconHref of
            Just _ ->
                Html.a

            Nothing ->
                Html.div
        )
        (case iconHref of
            Just h ->
                [ href h ]

            Nothing ->
                []
        )
        [ "block"
        , "opacity-30"
        , "text-inherit"
        ]
        [ icon 64 Inherit ]
    , slab
        (case iconHref of
            Just _ ->
                Html.a

            Nothing ->
                Html.div
        )
        (case textHref of
            Just h ->
                [ href h ]

            Nothing ->
                []
        )
        [ "block"
        , "leading-normal"
        , "mt-2"
        , "opacity-40"
        , "text-center"
        , "text-inherit"
        ]
        text
    , chunk
        [ "max-w-full"
        , "mt-4"
        ]
        nodes
    ]
        |> chunk
            [ "flex"
            , "flex-col"
            , "items-center"
            , "max-h-full"
            , "overflow-y-auto"
            , "px-4"
            , "py-8"
            , "w-full"
            ]
        |> List.singleton
        |> centeredContent


h1 : String -> Html msg
h1 text =
    slab
        Html.h1
        [ style "font-size" "13.5px" ]
        [ "all-small-caps"
        , "antialiased"
        , "bg-base06"
        , "inline-block"
        , "font-semibold"
        , "leading-tight"
        , "m-0"
        , "-top-px"
        , "overflow-hidden"
        , "pointer-events-none"
        , "px-2"
        , "py-1"
        , "relative"
        , "rounded-b"
        , "uppercase"
        , "text-sm"
        , "text-white"

        -- Dark mode
        ------------
        , "dark:bg-base01"
        , "dark:text-base05"
        ]
        [ Html.text text ]


h2 : String -> Html msg
h2 text =
    slab
        Html.h2
        []
        [ "font-bold"
        , "font-display"
        , "leading-tight"
        , "mb-8"
        , "mt-4"
        , "mx-auto"
        , "text-2xl"
        , "text-center"
        ]
        [ Html.text text ]


h3 : String -> Html msg
h3 text =
    slab
        Html.h2
        []
        [ "antialiased"
        , "font-bold"
        , "font-display"
        , "leading-tight"
        , "mb-8"
        , "mt-4"
        , "text-xl"
        ]
        [ Html.text text ]


inlineIcon : (Int -> Coloring -> Svg.Svg msg) -> Html msg
inlineIcon icon =
    inline
        [ "align-sub"
        , "inline-block"
        , "leading-0"
        , "mr-1"
        , "text-0"
        ]
        [ icon 14 Inherit ]


intro : Html msg -> Html msg
intro child =
    slab
        Html.p
        [ style "line-height" "1.75" ]
        [ "mb-6"
        , "mt-3"
        , "text-base05"
        , "text-sm"

        -- Dark mode
        ------------
        , "dark:text-base03"
        ]
        [ child ]


label : List (Html.Attribute msg) -> String -> Html msg
label attributes t =
    slab
        Html.label
        (style "font-size" "11.25px" :: attributes)
        [ "antialiased"
        , "block"
        , "font-bold"
        , "leading-normal"
        , "opacity-90"
        , "uppercase"
        ]
        [ Html.text t ]


link : { label : String, url : String } -> Html msg
link params =
    slab
        Html.a
        [ A.href params.url
        , A.target "_blank"
        ]
        [ "border-b-2"
        , "border-base04"
        , "inline-block"
        , "leading-none"
        , "no-underline"
        , "text-inherit"
        ]
        [ Html.text params.label ]


logoBackdrop : Html Never
logoBackdrop =
    chunk
        [ "logo-backdrop"

        --
        , "absolute"
        , "bg-cover"
        , "bg-no-repeat"
        , "h-0"
        , "left-full"
        , "opacity-025"
        , "pt-full"
        , "top-0"
        , "z-0"

        -- Dark mode
        ------------
        , "dark:opacity-40"
        ]
        []


receptacle : { scrolling : Bool } -> List (Html msg) -> Html msg
receptacle { scrolling } =
    chunk
        [ "absolute"
        , "bg-white"
        , "flex"
        , "flex-col"
        , "inset-0"
        , "overflow-x-hidden"
        , "scrolling-touch"
        , "z-50"

        -- Dark mode
        ------------
        , "dark:bg-darkest-hour"

        --
        , ifThenElse scrolling "overflow-y-auto" "overflow-y-hidden"
        ]


select : (String -> msg) -> List (Html msg) -> Html msg
select inputHandler options =
    chunk
        [ "max-w-md"
        , "mx-auto"
        , "relative"
        , "text-base05"
        , "w-full"

        --
        , "focus-within:text-black"

        -- Dark mode
        ------------
        , "dark:text-gray-600"
        , "dark:focus-within:text-base07"
        ]
        [ slab
            Html.select
            [ onInput inputHandler ]
            [ "appearance-none"
            , "border-b"
            , "border-l-0"
            , "border-r-0"
            , "border-t-0"
            , "border-gray-400"
            , "bg-transparent"
            , "block"
            , "leading-normal"
            , "m-0"
            , "outline-none"
            , "py-2"
            , "px-0"
            , "rounded-none"
            , "text-base01"
            , "text-lg"
            , "w-full"

            --
            , "focus:border-black"

            -- Dark mode
            ------------
            , "dark:border-base02"
            , "dark:text-gray-600"

            --
            , "dark:focus:border-base07"
            ]
            options
        , chunk
            [ "absolute"
            , "-translate-y-1/2"
            , "mt-px"
            , "right-0"
            , "text-0"
            , "top-1/2"
            , "transform"
            ]
            [ Icons.keyboard_arrow_down 20 Inherit ]
        ]


textArea : List (Html.Attribute msg) -> Html msg
textArea attributes =
    slab
        Html.textarea
        attributes
        [ "bg-white"
        , "block"
        , "leading-normal"
        , "mb-4"
        , "p-4"
        , "resize-none"
        , "rounded"
        , "text-base01"
        , "text-sm"
        , "w-full"

        --
        , "placeholder:text-base01"
        , "placeholder:text-opacity-40"

        -- Dark mode
        ------------
        , "dark:bg-darkest-hour"
        , "dark:text-gray-600"

        --
        , "dark:placeholder:text-gray-600"
        , "dark:placeholder:text-opacity-30"
        ]
        []


textButton : { label : String, onClick : msg } -> Html msg
textButton params =
    slab
        Html.button
        [ onClick params.onClick ]
        [ "appearance-none"
        , "bg-transparent"
        , "border-base04"
        , "border-b-2"
        , "text-inherit"
        , "leading-tight"
        , "m-0"
        , "p-0"
        , "cursor-pointer"
        ]
        [ Html.text params.label ]


textField : List (Html.Attribute msg) -> Html msg
textField attributes =
    slab
        Html.input
        attributes
        [ "appearance-none"
        , "border-b"
        , "border-l-0"
        , "border-r-0"
        , "border-t-0"
        , "border-gray-400"
        , "bg-transparent"
        , "block"
        , "leading-normal"
        , "mt-1"
        , "py-2"
        , "rounded-none"
        , "text-base01"
        , "text-sm"
        , "w-full"

        --
        , "focus:border-black"
        , "focus:outline-none"
        , "placeholder:text-base01"
        , "placeholder:text-opacity-40"

        -- Dark mode
        ------------
        , "dark:border-base02"
        , "dark:text-gray-600"

        --
        , "dark:focus:border-base07"
        , "dark:placeholder:text-gray-600"
        , "dark:placeholder:text-opacity-30"
        ]
        []


textFieldAlt : List (Html.Attribute msg) -> Html msg
textFieldAlt attributes =
    slab
        Html.input
        attributes
        [ "bg-white"
        , "block"
        , "leading-normal"
        , "mb-3"
        , "p-3"
        , "resize-none"
        , "rounded"
        , "text-base01"
        , "text-sm"
        , "w-full"

        -- Dark mode
        ------------
        , "dark:bg-darkest-hour"
        , "dark:text-gray-600"
        ]
        []
