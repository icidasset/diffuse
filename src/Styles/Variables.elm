module Variables exposing (..)

import Color exposing (Color, rgb, rgba)
import Element.Attributes exposing (Length)
import Style exposing (Font)
import Style.Font as Font
import Style.Scale as Scale


-- Scales


scaled : Int -> Float
scaled =
    Scale.modular 16 1.125


scaledPx : Int -> Length
scaledPx =
    scaled >> Element.Attributes.px



-- Colors


colors =
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


colorDerivatives =
    { consoleText = rgba 255 255 255 0.875
    , errorBorder = colors.base08
    , focusBorder = colors.base0D
    , inputBorder = rgb 225 225 225
    , logo = rgb 164 53 81
    , logoAlt = rgb 8 1 8
    , subtleBorder = rgb 238 238 238
    , sunset = rgb 206 186 183
    , -- States
      success = colors.base0B
    , error = colors.base08
    , -- Text
      text = colors.base01
    }



-- Fonts


baseFontSize : Float
baseFontSize =
    16


defaultFont : Font
defaultFont =
    Font.font "Overpass"


headerFont : Font
headerFont =
    Font.font "Montserrat"



-- Space properties


insulationWidth : Float
insulationWidth =
    840
