module Color.Ext exposing (setOpacity, toElmCssColor)

import Color exposing (Color)
import Css



-- 🔱


setOpacity : Float -> Color -> Color
setOpacity opacity color =
    color
        |> Color.toRgba
        |> (\c -> { c | alpha = opacity })
        |> Color.fromRgba


toElmCssColor : Color -> Css.Color
toElmCssColor color =
    color
        |> Color.toRgba
        |> (\{ red, green, blue, alpha } ->
                Css.rgba
                    (round <| red * 255)
                    (round <| green * 255)
                    (round <| blue * 255)
                    alpha
           )
