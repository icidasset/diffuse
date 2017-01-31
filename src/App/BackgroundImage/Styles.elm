module BackgroundImage.Styles exposing (..)

import Css exposing (..)


type Classes
    = BackgroundImage


type Ids
    = BackgroundImageFilter


styles : List Snippet
styles =
    [ class BackgroundImage
        [ backgroundColor (hex "#000")
        , height (vh 100)
        , left zero
        , position fixed
        , top zero
        , width (vw 100)
        , zIndex (int -10)
        ]
    ]
