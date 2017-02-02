module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, html)
import Traits exposing (..)
import Variables exposing (..)


-- COLLECTION

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
        [ Spinner.keyframes ]



-- LOCAL


type Classes
    = AuthenticationButton
    | AuthenticationButtonLogo
    | InTheMiddle


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
      --
    , class AuthenticationButton
        [ alignItems center
        , backgroundColor (hex "#fff")
        , borderRadius (px 3)
        , cursor pointer
        , displayFlex
        , lineHeight (gr 1.5)
        , padding2 (gr 1) (gr 1.5)
          --
        , descendants
            [ class AuthenticationButtonLogo
                [ height (px 22)
                , marginRight (gr 1)
                , width (px 22)
                ]
            ]
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
