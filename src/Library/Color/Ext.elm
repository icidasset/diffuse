module Color.Ext exposing (toElmCssColor)

import Color
import Css


toElmCssColor : Color.Color -> Css.Color
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
