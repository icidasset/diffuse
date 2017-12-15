module StylesOld exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Media exposing (withMedia)
import Traits exposing (..)
import Variables exposing (..)


-- Children

import Alfred.Styles as Alfred
import ContextMenu.StylesOld as ContextMenu
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
        |> List.append Alfred.styles
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
    | ButtonSmall
    | ButtonSubtle
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
    | OverlayWithCursor
    | Shell


stylesLocal : List Snippet
stylesLocal =
    [ ------------------------------------------------------
      -- <html> ✅
      ------------------------------------------------------
      html
        [ fontSize (px 14)
        , withMedia [ tablet ] [ fontSize (px baseFontSize) ]
        ]

    ------------------------------------------------------
    -- Insulation
    --
    -- > Main wrapper used for the layout with the music
    --   controls visible.
    ------------------------------------------------------
    , class Insulation
        [ backgroundColor (hex "#fff")
        , borderRadius (px 3)
        , boxShadow4 (px 0) (px 2) (px 4) (rgba 0 0 0 0.2)
        , displayFlex
        , flex (int 1)
        , margin3 (gr 10) auto zero
        , maxWidth (px insulationWidth)
        , minHeight (px 218)
        , overflow hidden
        , position relative
        , width (pct 100)

        -- Nested insulation
        , children
            [ class Insulation
                [ bottom zero
                , left zero
                , margin zero
                , overflow visible
                , position absolute
                , right zero
                , top zero
                , zIndex (int 9)
                ]
            ]
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
        , borderBottomLeftRadius (px 3)
        , borderBottomRightRadius (px 3)
        , color (rgb 255 255 255)
        , display Css.table
        , fontSize (Css.rem 0.725)
        , fontWeight (int 700)
        , letterSpacing (Css.em 0.0125)
        , marginBottom (gr 3)
        , marginTop (px -1)
        , padding3 (basem 4) (basem 10) (basem 3)
        , textTransform uppercase
        ]
    , h2
        [ fontSize (Css.rem 1.4)
        , Traits.headerFont
        , margin3 (gr -2) zero (gr 4)
        , textAlign center
        ]
    , h3
        [ fontSize (Css.rem 1.4)
        , Traits.headerFont
        , margin3 (gr -2) zero (gr 6)
        , textAlign left
        ]
    , h4
        [ color (cssColor colors.base06)
        , fontSize (Css.rem 0.75)
        , fontWeight (int 500)
        , Traits.headerFont
        , textTransform uppercase

        --
        , descendants
            [ svg
                [ display inlineBlock
                , height (Css.em 1.1)
                , marginRight (px 8)
                , transform (translateY (px -1))
                , verticalAlign middle
                , width (Css.em 1.1)
                ]
            , selector "g"
                [ fill currentColor
                ]
            ]
        ]
    , a
        [ color inherit
        , textDecoration none
        ]
    , label
        [ cursor inherit
        ]

    ------------------------------------------------------
    -- <🎃> Buttons
    ------------------------------------------------------
    , (each [ class Button, button ])
        [ backgroundColor transparent
        , border3 (px 2) solid currentColor
        , borderRadius (px 3)
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
        , textTransform none

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

        --
        , adjacentSiblings
            [ (each [ class Button, button ])
                [ marginLeft (gr 1)
                ]
            ]
        ]

    -- Additional button styles
    --
    , class ButtonSmall
        [ fontSize (Css.rem 0.85)
        , height (gr 5)
        , lineHeight (gr 5)
        , padding3 (px 1) (px 8) zero
        ]
    , class ButtonSubtle
        [ color (cssColor colors.base05)
        , fontWeight (int 600)
        ]

    --                          --
    --                          --
    --  OTHER BASIC COMPONENTS  --
    --                          --
    --                          --
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
            , p
                [ margin zero
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
        [ backgroundImage (url "/images/icon-dark.svg")
        , backgroundPosition2 (px -124) (pct 53.75)
        , backgroundRepeat noRepeat
        , backgroundSize cover
        , bottom zero
        , left zero
        , opacity (num 0.025)
        , position absolute
        , right zero
        , top zero
        ]
    ]
