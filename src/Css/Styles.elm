module Styles exposing (..)

import Color.Manipulate
import Css exposing (..)
import Css.Elements exposing (..)
import Traits exposing (..)
import Variables exposing (..)


-- Children

import ContextMenu.Styles as ContextMenu
import Console.Styles as Console
import Equalizer.Styles as Equalizer
import Form.Styles as Form
import List.Styles as List
import Navigation.Styles as Navigation
import Spinner.Styles as Spinner
import Tracks.Styles as Tracks


styles : List Snippet
styles =
    stylesLocal
        |> List.append ContextMenu.styles
        |> List.append Console.styles
        |> List.append Equalizer.styles
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
    | BackgroundImage
    | Basic
    | Button
    | Columns
    | ContentBox
    | EmptyState
    | Important
    | Insulation
    | InsulationCentered
    | InsulationContent
    | InsulationFlexContent
    | InTheMiddle
    | Intro
    | LogoBackdrop
    | Overlay
    | Shell


stylesLocal : List Snippet
stylesLocal =
    [ ------------------------------------------------------
      -- <html>
      ------------------------------------------------------
      html
        [ fontSize (px 14)
        ]

    --
    , mediaQuery "screen and ( min-width: 700px )"
        [ html
            [ fontSize (px baseFontSize)
            ]
        ]

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
        , property "-moz-osx-font-smoothing" "grayscale"
        , property "font-smooth" "always"
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
        -- , backgroundPosition2 (pct 50) (pct 19)
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
        , overflow hidden
        , position relative
        , width (pct 100)
        ]
    , class InsulationCentered
        [ alignItems center
        , justifyContent center
        ]
    , class InsulationContent
        [ flex (int 1)
        , overflowX hidden
        , overflowY auto
        ]
    , class InsulationFlexContent
        [ displayFlex
        , flexDirection column
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
        [ padding3 zero (gr 4) (gr 6)
        ]

    ------------------------------------------------------
    -- <🎃>
    ------------------------------------------------------
    , h1
        [ backgroundColor (cssColor colors.base06)
        , borderBottomLeftRadius (px 4)
        , borderBottomRightRadius (px 4)
        , color (rgb 255 255 255)
        , display Css.table
        , fontSize (gr 2)
        , fontWeight (int 700)
        , letterSpacing (Css.em 0.0125)
        , marginBottom (gr 3)
        , marginTop (px -1)
        , padding3 (basem 4) (basem 10) (basem 3)
        , textTransform uppercase
        ]
    , h2
        [ fontSize (Css.rem 1.4)
        , headerFont
        , margin3 (gr -2) zero (gr 4)
        , textAlign center
        ]
    , h3
        [ fontSize (Css.rem 1.4)
        , headerFont
        , margin3 (gr -2) zero (gr 6)
        , textAlign left
        ]
    , a
        [ color inherit
        , textDecoration none
        ]
    , label
        [ cursor inherit
        ]
    , svg
        [ height (Css.em 1)
        , width (Css.em 1)
        ]

    ------------------------------------------------------
    -- <🎃> Buttons
    ------------------------------------------------------
    , (each [ class Button, button ])
        [ backgroundColor transparent
        , border3 (px 2) solid (cssColor colorDerivatives.success)
        , borderRadius borderRadiuses.smallElements
        , boxSizing contentBox
        , color (cssColor colorDerivatives.success)
        , cursor pointer
        , display inlineBlock
        , fontFamily inherit
        , fontSize (Css.rem 0.95)
        , fontWeight (int 700)
        , height (gr 6)
        , lineHeight (gr 6)
        , padding3 (px 1) (gr 2) zero

        --
        , focus
            [ outline none
            ]

        --
        , descendants
            [ svg
                [ fontSize (Css.em 1.2)
                , marginTop (px -2)
                , verticalAlign middle
                ]
            , selector "g"
                [ fill currentColor
                ]
            ]
        ]

    --                          --
    --                          --
    --  OTHER BASIC COMPONENTS  --
    --                          --
    --                          --
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
        , height (px 22)
        , lineHeight (gr 3)
        , marginBottom (gr 2)
        , padding2 (gr 2) (gr 3)
        , property "padding-top" "calc(.75rem + 1px)"
        , width (px 220)

        --
        , lastChild
            [ marginBottom zero
            ]

        --
        , descendants
            [ span
                [ lineHeight (Css.rem 1)
                ]
            , svg
                [ marginRight (gr 1)
                , transform none
                , width (basem 22)
                ]
            ]
        ]

    ------------------------------------------------------
    -- Basic wrapper
    ------------------------------------------------------
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

    ------------------------------------------------------
    -- Columns
    ------------------------------------------------------
    , class Columns
        [ property "columns" "2"
        , property "column-gap" (.value (gr 8))

        --
        , children
            [ div
                [ property "-webkit-column-break-inside" "avoid"
                , property "break-inside" "avoid"
                , property "page-break-inside" "avoid"
                ]
            ]
        ]

    ------------------------------------------------------
    -- Empty state
    ------------------------------------------------------
    , class EmptyState
        [ color (cssColor colors.base06)
        , fontWeight (int 600)
        , left (pct 50)
        , lineHeight (num 1.45)
        , marginTop (gr 3)
        , position absolute
        , textAlign center
        , top (pct 50)
        , transform (translate2 (pct -50) (pct -50))

        --
        , descendants
            [ svg
                [ fontSize (basem 64)
                , marginBottom (gr 1)
                ]
            , selector "g"
                [ fill currentColor
                ]
            ]
        ]

    ------------------------------------------------------
    -- Important text
    ------------------------------------------------------
    , class Important
        [ alignItems center
        , border3
            (px 2)
            solid
            (colors.base08
                |> cssColor
            )
        , borderRadius (px 3)
        , color (cssColor colors.base08)
        , displayFlex
        , fontWeight (int 700)
        , lineHeight (int 1)
        , padding2 (gr 2) (gr 2)

        --
        , descendants
            [ selector "svg g"
                [ fill currentColor ]
            ]
        ]

    ------------------------------------------------------
    -- Intro
    ------------------------------------------------------
    , class Intro
        [ fontSize (Css.em 0.9)
        , lineHeight (num 1.6)
        , marginBottom (gr 6)
        , marginTop (gr 4)
        , opacity (num 0.475)

        --
        , descendants
            [ a
                [ borderBottom3 (px 2) solid (cssColor colors.base0A)
                ]
            , svg
                [ marginRight (gr 1)
                , verticalAlign middle
                ]
            ]
        ]

    ------------------------------------------------------
    -- Logo backdrop
    ------------------------------------------------------
    , class LogoBackdrop
        [ backgroundImage (url "/images/icon.svg")
        , backgroundPosition2 (px -124) (pct 53.75)
        , backgroundRepeat noRepeat
        , backgroundSize cover
        , bottom zero
        , left zero
        , opacity (num 0.05)
        , position absolute
        , right zero
        , top zero
        ]

    ------------------------------------------------------
    -- Overlay
    ------------------------------------------------------
    , class Overlay
        [ backgroundColor (rgba 0 0 0 0.25)
        , height (vh 100)
        , left zero
        , opacity zero
        , position fixed
        , property "pointer-events" "none"
        , property "transition" "opacity 1s ease"
        , top zero
        , width (vw 100)
        , zIndex (int 900)
        ]
    ]
