module BackgroundImage.Styles exposing (..)

import Css exposing (..)
import Css.Colors exposing (black)


type Classes
    = BackgroundImage


type Ids
    = BackgroundImageFilter


styles : List Snippet
styles =
    [ class BackgroundImage
        [ backgroundColor black
        , height (vh 100)
        , left zero
        , position fixed
        , top zero
        , width (vw 100)
        , zIndex (int -10)
        ]
    ]
