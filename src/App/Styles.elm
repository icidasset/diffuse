module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, html)
import Traits exposing (..)
import Variables exposing (..)


-- Collection

import BackgroundImage.Styles as BackgroundImage
import Spinner.Styles as Spinner


styles : List Snippet
styles =
    stylesLocal
        |> List.append BackgroundImage.styles
        |> List.append Spinner.styles


keyframes : String
keyframes =
    String.concat
        [ Spinner.keyframes
        ]



-- Local


type Classes
    = InTheMiddle


stylesLocal : List Snippet
stylesLocal =
    [ ------------------------------------------------------
      -- <html>
      ------------------------------------------------------
      html
        [ fontSize (px 16) ]
      ------------------------------------------------------
      -- <body>
      ------------------------------------------------------
    , body
        [ backgroundColor (hex "#fff")
        , color (hex colorDerivatives.textColor)
        , defaultFont
        , fontSize (Css.rem 1)
        , lineHeight (Css.num 1.75)
        , textRendering optimizeLegibility
        ]
      ------------------------------------------------------
      -- In the middle
      ------------------------------------------------------
    , class InTheMiddle
        [ alignItems center
        , displayFlex
        , height (vh 100)
        , justifyContent center
        ]
    ]
