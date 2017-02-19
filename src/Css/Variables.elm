module Variables exposing (..)

import Css


colors =
    { base00 = "#2f1e2e"
    , base01 = "#41323f"
    , base02 = "#4f424c"
    , base03 = "#776e71"
    , base04 = "#8d8687"
    , base05 = "#a39e9b"
    , base06 = "#b9b6b0"
    , base07 = "#e7e9db"
    , base08 = "#ef6155"
    , base09 = "#f99b15"
    , base0A = "#fec418"
    , base0B = "#48b685"
    , base0C = "#5bc4bf"
    , base0D = "#06b6ef"
    , base0E = "#815ba4"
    , base0F = "#e96ba8"
    }


colorDerivatives =
    { subtleBorder = "#eee"
    , inputBorder = "#ccc"
    , focusBorder = colors.base0D
    , errorBorder = colors.base08
    , -- States
      success = colors.base0B
    , error = colors.base08
    , -- Text
      text = colors.base01
    }


borderRadiuses =
    { smallElements = (Css.px 3)
    }
