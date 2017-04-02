module Traits exposing (..)

import Color
import Css exposing (..)


-- Css / Grid


gr : Int -> Rem
gr number =
    Css.rem (0.375 * (toFloat number))



-- Css / Queries


querySmall : List Snippet -> Snippet
querySmall =
    mediaQuery "screen and (min-width: 480px)"


queryMedium : List Snippet -> Snippet
queryMedium =
    mediaQuery "screen and (min-width: 768px)"


queryLarge : List Snippet -> Snippet
queryLarge =
    mediaQuery "screen and (min-width: 1024px)"



-- Css / Mixins


defaultFont : Mixin
defaultFont =
    mixin [ fontFamilies [ "Overpass", "sans-serif" ] ]



-- Css / Colors


cssColor : Color.Color -> Css.Color
cssColor theColor =
    let
        values =
            Color.toRgb theColor
    in
        Css.rgba values.red values.green values.blue values.alpha
