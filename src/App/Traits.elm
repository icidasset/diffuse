module Traits exposing (..)

import Css exposing (..)


-- Css / Grid


gr : Float -> Rem
gr number =
    Css.rem (0.75 * number)



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
