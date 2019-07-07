module UI.Css exposing (notSmallMediaQuery)

import Css exposing (em)
import Css.Media exposing (..)



-- 🔱


notSmallMediaQuery : MediaQuery
notSmallMediaQuery =
    only screen [ minWidth (em 30) ]
