module Traits exposing (..)

import Color
import Css exposing (..)
import Css.Media exposing (..)
import Variables exposing (baseFontSize)


-- Css / Fonts


basem : Float -> Rem
basem number =
    Css.rem (number / baseFontSize)


intoRem : Float -> String
intoRem number =
    .value (basem number)



-- Css / Grid


gr : Int -> Rem
gr number =
    Css.rem (0.375 * (toFloat number))



-- Css / Queries


iPhone6 : MediaQuery
iPhone6 =
    only screen [ Css.Media.minWidth (px 480) ]


tablet : MediaQuery
tablet =
    only screen [ Css.Media.minWidth (px 700) ]



-- Css / Mixins


defaultFont : Style
defaultFont =
    batch [ fontFamilies [ "Overpass", "sans-serif" ] ]


headerFont : Style
headerFont =
    batch [ fontFamilies [ "Montserrat", "sans-serif" ] ]


disableSelect : Style
disableSelect =
    batch
        [ property "-webkit-user-select" "none"
        , property "user-select" "none"
        ]



-- Css / Colors


cssColor : Color.Color -> Css.Color
cssColor theColor =
    let
        values =
            Color.toRgb theColor
    in
        Css.rgba values.red values.green values.blue values.alpha


cssColorOpac : Float -> Color.Color -> Css.Color
cssColorOpac num clr =
    clr
        |> colorOpac num
        |> cssColor


colorOpac : Float -> Color.Color -> Color.Color
colorOpac num clr =
    clr
        |> Color.toRgb
        |> (\c -> { c | alpha = num })
        |> (\c -> Color.rgba c.red c.green c.blue c.alpha)
