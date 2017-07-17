module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Traits exposing (..)
import Variables exposing (..)


-- Children

import Console.Styles as Console
import Form.Styles as Form
import List.Styles as List
import Navigation.Styles as Navigation
import Spinner.Styles as Spinner
import Tracks.Styles as Tracks


styles : List Snippet
styles =
    stylesLocal
        |> List.append Console.styles
        |> List.append Form.styles
        |> List.append List.styles
        |> List.append Navigation.styles
        |> List.append Spinner.styles
        |> List.append Tracks.styles


keyframes : String
keyframes =
    String.concat
        [ Spinner.keyframes ]



-- 🦄


type Classes
    = AuthenticationButton
    | AuthenticationButtonLogo
    | BackgroundImage
    | Basic
    | Button
    | ContentBox
    | Insulation
    | InsulationContent
    | InTheMiddle
    | Intro
    | Shell


stylesLocal : List Snippet
stylesLocal =
    [ ------------------------------------------------------
      -- <html>
      ------------------------------------------------------
      html
        [ fontSize (px baseFontSize) ]

    ------------------------------------------------------
    -- <body>
    ------------------------------------------------------
    , body
        [ backgroundColor (hex "#000")
        , color (cssColor colorDerivatives.text)
        , defaultFont
        , fontSize (Css.rem 1)
        , lineHeight (Css.num 1.75)
        , overflow hidden
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
        [ backgroundImage (url "images/Background/4.jpg")
        , backgroundPosition bottom
        , backgroundSize cover

        -- For: 1.jpg
        --, backgroundPosition2 (pct 50) (pct 19)
        -- , backgroundSize (pct 110)
        --
        , backgroundRepeat noRepeat
        , height (vh 100)
        , left zero
        , position fixed
        , top zero
        , width (vw 100)
        , zIndex (int -10)
        ]

    ------------------------------------------------------
    -- Shell
    --
    -- > Surrounding node
    ------------------------------------------------------
    , class Shell
        [ displayFlex
        , flexDirection column
        , height (vh 100)
        , padding2 zero (gr 3)
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
        , displayFlex
        , flex (int 1)
        , margin3 (gr 10) auto zero
        , maxWidth insulationWidth
        , position relative
        , width (pct 100)
        ]
    , class InsulationContent
        [ flex (int 1)
        , overflowX hidden
        , overflowY auto
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
        [ padding3 (gr 4) (gr 4) (gr 6)
        ]

    ------------------------------------------------------
    -- <🎃>
    ------------------------------------------------------
    , h1
        [ fontSize (Css.rem 1.675)
        , fontWeight (int 600)
        , headerFont
        , letterSpacing (Css.em -0.025)
        , marginTop zero
        ]
    , a
        [ color inherit
        , textDecoration none
        ]
    , label
        [ cursor inherit
        ]
    , class Basic
        [ color (hex "#fff")
        , lineHeight (num 1.5)
        , textAlign center

        --
        , descendants
            [ svg
                [ display inlineBlock
                , marginRight (gr 1)
                , transform (translateY (px 2))
                ]
            ]
        ]
    , class Intro
        [ fontSize (Css.em 0.95)
        , fontWeight (int 600)
        , headerFont
        , lineHeight (num 1.55)
        , marginBottom (gr 6)
        , opacity (num 0.475)

        --
        , descendants
            [ a
                [ borderBottom3 (px 2) solid (cssColor colors.base0A)
                ]
            ]
        ]

    ------------------------------------------------------
    -- Buttons
    ------------------------------------------------------
    , (each [ class Button, button ])
        [ backgroundColor transparent
        , border3 (px 1) solid (cssColor colorDerivatives.success)
        , borderRadius borderRadiuses.smallElements
        , boxSizing contentBox
        , color (cssColor colorDerivatives.success)
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
                , marginRight (gr 1)
                , transform none
                , width (px 22)
                ]
            ]
        ]
    ]
