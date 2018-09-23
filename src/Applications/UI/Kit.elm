module UI.Kit exposing (colorKit, colors, defaultFont, headerFont, insulationWidth)

import Color



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
