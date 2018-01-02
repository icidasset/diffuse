module Color.Ext exposing (..)

import Color exposing (Color)


setAlpha : Float -> Color -> Color
setAlpha alpha color =
    color
        |> Color.toRgb
        |> (\c -> { c | alpha = alpha })
        |> (\c -> Color.rgba c.red c.green c.blue c.alpha)
