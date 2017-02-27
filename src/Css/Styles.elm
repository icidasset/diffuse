module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Traits exposing (..)
import Variables exposing (..)


-- Children

import Form.Styles as Form
import Navigation.Styles as Navigation
import Spinner.Styles as Spinner


styles : List Snippet
styles =
    stylesLocal
        |> List.append Form.styles
        |> List.append Navigation.styles
        |> List.append Spinner.styles


keyframes : String
keyframes =
    String.concat
        [ Spinner.keyframes ]



-- 🦄


type Classes
    = AuthenticationButton
    | AuthenticationButtonLogo
    | BackgroundImage
    | Button
    | ContentBox
    | Insulation
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
        , color (hex colorDerivatives.text)
        , defaultFont
        , fontSize (Css.rem 1)
        , lineHeight (Css.num 1.75)
        , textRendering optimizeLegibility
          --
        , property "-webkit-font-smoothing" "antialiased"
        , property "-moz-font-smoothing" "grayscale"
        ]
      ------------------------------------------------------
      -- Background image
      --
      -- > Not on the <body> for a reason.
      ------------------------------------------------------
    , class BackgroundImage
        [ backgroundImage (url "images/Background/1.jpg")
        , backgroundPosition center
        , backgroundRepeat noRepeat
        , backgroundSize (pct 110)
        , height (vh 100)
        , left zero
        , position fixed
        , top zero
        , width (vw 100)
        , zIndex (int -10)
        ]
      ------------------------------------------------------
      -- Insulation
      --
      -- > Main wrapper used for the layout with the music
      --   controls visible.
      ------------------------------------------------------
    , class Insulation
        [ backgroundColor (hex "#fff")
        , boxShadow4 (px 0) (px 2) (px 4) (rgba 0 0 0 0.2)
        , margin3 (gr 10) auto (gr 20)
        , maxWidth (gr 100)
        , position relative
        ]
      ------------------------------------------------------
      -- In the middle
      --
      -- > Vertically and horizontally center stuff.
      ------------------------------------------------------
    , class InTheMiddle
        [ alignItems center
        , displayFlex
        , height (vh 100)
        , justifyContent center
        ]
      ------------------------------------------------------
      -- Content box
      --
      -- > Just a box with some padding and stuff.
      --   Mainly used for containing text elements.
      ------------------------------------------------------
    , class ContentBox
        [ padding2 (gr 6) (gr 4)
        ]
      ------------------------------------------------------
      -- <🎃>
      ------------------------------------------------------
    , h1
        [ fontSize (Css.rem 1.55)
        , fontWeight (int 800)
        , letterSpacing (Css.em -0.025)
        , marginTop zero
        ]
    , a
        [ color inherit
        , textDecoration none
        ]
      ------------------------------------------------------
      -- Buttons
      ------------------------------------------------------
    , (each [ class Button, button ])
        [ backgroundColor transparent
        , border3 (px 1) solid (hex colorDerivatives.success)
        , borderRadius borderRadiuses.smallElements
        , boxSizing contentBox
        , color (hex colorDerivatives.success)
        , cursor pointer
        , display inlineBlock
        , fontFamily inherit
        , fontSize (Css.rem 0.95)
        , fontWeight (int 600)
        , height (gr 6)
        , lineHeight (gr 6)
        , padding3 (px 1) (gr 2) zero
          --
        , focus
            [ outline none
            ]
        ]
      ------------------------------------------------------
      -- Authentication button
      ------------------------------------------------------
    , class AuthenticationButton
        [ alignItems center
        , backgroundColor (rgba 0 0 0 0.45)
        , borderRadius borderRadiuses.smallElements
        , boxShadow4 (px 0) (px 0) (px 20) (rgba 255 255 255 0.05)
        , color (hex "#fff")
        , cursor pointer
        , displayFlex
        , fontSize (Css.rem 0.95)
        , lineHeight (gr 3)
        , padding2 (gr 2) (gr 3)
        , property "padding-top" "calc(.75rem + 1px)"
          --
        , descendants
            [ class AuthenticationButtonLogo
                [ height (px 22)
                , marginRight (gr 2)
                , width (px 22)
                ]
            ]
        ]
    ]
