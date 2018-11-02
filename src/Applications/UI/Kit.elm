module UI.Kit exposing (canister, colorKit, colors, defaultFont, h1, h2, headerFont, insulationWidth, intro)

import Chunky exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes exposing (style)
import Tachyons.Classes as T



-- Colors


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
    }


colors =
    { errorBorder = colorKit.base08
    , focusBorder = colorKit.base0D
    , inputBorder = rgb 225 225 225
    , subtleBorder = rgb 238 238 238
    , -- States
      success = colorKit.base0B
    , error = colorKit.base08
    , -- Text
      text = colorKit.base01
    }


rgb =
    Color.rgb255



-- Fonts


defaultFont : String
defaultFont =
    "Source Sans Pro"


headerFont : String
headerFont =
    "Montserrat"



-- Space properties


insulationWidth : Float
insulationWidth =
    840



-- Nodes


canister : List (Html msg) -> Html msg
canister children =
    chunk
        [ T.mh1, T.ph3 ]
        children


h1 : String -> Html msg
h1 text =
    slab
        Html.h1
        [ style "background-color" (Color.toCssString colorKit.base06)
        , style "font-size" "11.25px"
        , style "letter-spacing" "0.25px"
        , style "top" "-1px"
        ]
        [ T.br2
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
        [ style "font-family" headerFont ]
        [ T.f3
        , T.fw7
        , T.lh_title
        , T.mb4
        , T.mt0
        ]
        [ Html.text text ]


intro : Html msg -> Html msg
intro child =
    slab
        Html.p
        [ style "color" (Color.toCssString colorKit.base05)
        , style "line-height" "1.75em"
        ]
        [ T.f6
        , T.mv3
        , T.pv1
        ]
        [ child ]
