module UI.Css exposing (largeMediaQuery, mediumMediaQuery, notSmallMediaQuery)

import Css exposing (em)
import Css.Media exposing (..)



-- 🔱


notSmallMediaQuery : MediaQuery
notSmallMediaQuery =
    only screen [ minWidth (em 30) ]


mediumMediaQuery : MediaQuery
mediumMediaQuery =
    only screen [ minWidth (em 30), maxWidth (em 60) ]


largeMediaQuery : MediaQuery
largeMediaQuery =
    only screen [ minWidth (em 60) ]
