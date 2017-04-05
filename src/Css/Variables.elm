module Variables exposing (..)

import Color
import Css
import Traits exposing (gr)


colors =
    { base00 = Color.rgb 47 30 46
    , base01 = Color.rgb 65 50 63
    , base02 = Color.rgb 79 66 76
    , base03 = Color.rgb 119 110 113
    , base04 = Color.rgb 141 134 135
    , base05 = Color.rgb 163 158 155
    , base06 = Color.rgb 185 182 176
    , base07 = Color.rgb 231 233 219
    , base08 = Color.rgb 239 97 85
    , base09 = Color.rgb 249 155 21
    , base0A = Color.rgb 254 196 24
    , base0B = Color.rgb 72 182 133
    , base0C = Color.rgb 91 196 191
    , base0D = Color.rgb 6 182 239
    , base0E = Color.rgb 129 91 164
    , base0F = Color.rgb 233 107 168
    }


colorDerivatives =
    { consoleText = Color.rgba 255 255 255 0.875
    , errorBorder = colors.base08
    , focusBorder = colors.base0D
    , inputBorder = Color.rgb 204 204 204
    , subtleBorder = Color.rgb 238 238 238
    , -- States
      success = colors.base0B
    , error = colors.base08
    , -- Text
      text = colors.base01
    }


borderRadiuses =
    { smallElements = (Css.px 3)
    }


insulationWidth =
    gr 140
