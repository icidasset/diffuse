module Styles exposing (Styles(..), styles)

import Color exposing (..)
import Element.Attributes
import Style exposing (..)
import Style.Background as Background
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Style.Sheet as Sheet
import Style.Sheet.Ext as Sheet
import Style.Transition as Transition
import Time
import Variables exposing (..)
import Variations exposing (Variations)


-- Children

import Alfred.Styles as Alfred
import Console.Styles as Console
import ContextMenu.Styles as ContextMenu
import Equalizer.Styles as Equalizer
import Form.Styles as Form
import List.Styles as List
import Navigation.Styles as Navigation
import Tracks.Styles as Tracks


-- ⚗️


type Styles
    = -- 🚀
      Root
      -- Basics
    | Faulty
    | InlineMessage
    | Intro
    | Message
    | Selected
    | WithoutLineHeight
      -- Buttons
    | AuthenticationButton
    | Button
    | ImportantButton
    | SubtleButton
      -- Containers
    | AuthenticationOptions
    | Columns
    | ColumnsChild
    | EmptyState
    | Insulation
    | NestedInsulation
      -- Decorations
    | LogoBackdrop
    | LogoFull
    | Overlay
      -- Headings
    | H1
    | H2
    | H3
    | H4
    | H5
    | H6
      -- 🌳
    | Alfred Alfred.Styles
    | Console Console.Styles
    | ContextMenu ContextMenu.Styles
    | Equalizer Equalizer.Styles
    | Form Form.Styles
    | List List.Styles
    | Navigation Navigation.Styles
    | Tracks Tracks.Styles
      -- 💀
    | Zed



-- 🍯


styles : StyleSheet Styles Variations
styles =
    Style.styleSheet
        [ root
        , zed

        --
        , Sheet.mix basics
        , Sheet.mix buttons
        , Sheet.mix containers
        , Sheet.mix decorations
        , Sheet.mix headings

        --
        , Sheet.mixChild Alfred Alfred.styles
        , Sheet.mixChild Console Console.styles
        , Sheet.mixChild ContextMenu ContextMenu.styles
        , Sheet.mixChild Equalizer Equalizer.styles
        , Sheet.mixChild Form Form.styles
        , Sheet.mixChild List List.styles
        , Sheet.mixChild Navigation Navigation.styles
        , Sheet.mixChild Tracks Tracks.styles
        ]



-- 🚀


root : Style Styles Variations
root =
    style Root
        [ Color.text colorDerivatives.text
        , Font.lineHeight 1.75
        , Font.size (scaled 1)
        , Font.typeface [ defaultFont, Font.sansSerif ]

        -- Font smoothing
        , prop "-webkit-font-smoothing" "antialiased"
        , prop "-moz-osx-font-smoothing" "grayscale"
        , prop "font-smooth" "always"

        -- Text rendering
        , prop "text-rendering" "optimizeLegibility"
        ]



-- 💀


zed : Style Styles Variations
zed =
    --
    -- Who's motorcycle is this?
    -- > It's a chopper baby.
    -- Who's chopper is this?
    -- > Zed's.
    -- Who's Zed?
    -- > Zed's dead baby, Zed's dead.
    --
    style Zed []



-- Basics


basics : List (Style Styles Variations)
basics =
    [ style Faulty [ Color.text colorDerivatives.error ]
    , style InlineMessage [ Font.italic, Font.size (scaled -2) ]
    , style Intro [ Font.size (scaled -1), opacity 0.475 ]
    , style Message [ Color.text Color.white ]
    , style Selected [ Color.text colors.base08 ]
    , style WithoutLineHeight [ Font.lineHeight 0 ]
    ]



-- Buttons


buttons : List (Style Styles Variations)
buttons =
    let
        default =
            [ Border.all 2
            , Border.rounded 3
            , Font.size (scaled -1)
            , Font.weight 700

            --
            , cursor "pointer"
            ]
    in
        [ -----------------------------------
          -- Authentication button
          -----------------------------------
          style AuthenticationButton
            [ Border.bottom 1
            , Color.border colors.base07
            , Color.text colorDerivatives.text
            , Font.size (scaled -1)

            --
            , cursor "pointer"
            , pseudo "last-child" [ Border.bottom 0 ]
            ]

        -----------------------------------
        -- Important button
        -----------------------------------
        , List.append
            (default)
            [ Color.border colors.base08
            , Color.text colors.base08
            , Style.opacity 0.75
            ]
            |> style ImportantButton

        -----------------------------------
        -- Standard button
        -----------------------------------
        , List.append
            (default)
            [ Color.border colorDerivatives.success
            , Color.text colorDerivatives.success
            ]
            |> style Button

        -----------------------------------
        -- Subtle button
        -----------------------------------
        , List.append
            (default)
            [ Color.border colors.base05
            , Color.text colors.base05
            ]
            |> style SubtleButton
        ]



-- Containers


containers : List (Style Styles Variations)
containers =
    [ -----------------------------------
      -- Authentication options
      -----------------------------------
      style AuthenticationOptions
        [ Border.rounded 4
        , Color.background white
        ]

    -----------------------------------
    -- Columns
    -----------------------------------
    , style Columns
        [ prop "columns" "2"
        , prop "column-gap" (scaledStr 10)
        ]
    , style ColumnsChild
        [ prop "display" "block !important"

        --
        , prop "-webkit-column-break-inside" "avoid"
        , prop "break-inside" "avoid"
        , prop "page-break-inside" "avoid"
        ]

    -----------------------------------
    -- Empty state
    -----------------------------------
    , style EmptyState
        [ Color.text colors.base06
        , Font.center
        , Font.lineHeight 1.45
        , Font.weight 600
        ]

    -----------------------------------
    -- Insulation
    -----------------------------------
    , style Insulation
        [ Border.rounded 3
        , Color.background white

        -- Shadows
        , Shadow.box
            { offset = ( 0, 2 )
            , size = 0
            , blur = 4
            , color = rgba 0 0 0 0.2
            }
        ]

    -----------------------------------
    -- Nested insulation
    -----------------------------------
    , style NestedInsulation
        [ Border.rounded 3
        , Color.background white
        , Style.prop "-webkit-overflow-scrolling" "touch"
        ]
    ]



-- Decorations


decorations : List (Style Styles Variations)
decorations =
    [ -----------------------------------
      -- Logo backdrop
      -----------------------------------
      style LogoBackdrop
        [ Style.opacity 0.025
        , Style.prop "overflow" "hidden"

        --
        , pseudo "after"
            [ Style.prop "background-position" "-43.5% 98px !important"

            --
            , Background.imageWith
                { src = "/images/diffuse__icon-dark.svg"
                , position = ( 0, 0 )
                , repeat = Background.space
                , size = Background.cover
                }

            --
            , Style.prop "content" "''"
            , Style.prop "height" "0"
            , Style.prop "left" "100%"
            , Style.prop "padding-top" "100%"
            , Style.prop "position" "absolute"
            , Style.prop "top" "0"
            , Style.prop "transform" "rotate(90deg)"
            , Style.prop "transform-origin" "left top"
            , Style.prop "width" "105vh"
            ]
        ]

    -----------------------------------
    -- Logo full
    -----------------------------------
    , style LogoFull
        [ Background.imageWith
            { src = "/images/diffuse-light.svg"
            , position = ( 0, 0 )
            , repeat = Background.noRepeat
            , size = Background.contain
            }

        --
        , Style.prop "background-position" "center"
        , Style.prop "background-size" "contain"
        ]

    -----------------------------------
    -- Overlay
    -----------------------------------
    , style Overlay
        [ Color.background (rgba 0 0 0 0.25)
        , Style.opacity 0

        -- Transitions
        , { delay = 0
          , duration = Time.second * 1
          , easing = "ease"
          , props = [ "opacity" ]
          }
            |> List.singleton
            |> Transition.transitions
        ]
    ]



-- Headings


headings : List (Style Styles Variations)
headings =
    [ -----------------------------------
      -- H1
      -----------------------------------
      style H1
        [ Border.roundBottomLeft 3
        , Border.roundBottomRight 3
        , Color.background colors.base06
        , Color.text Color.white
        , Font.letterSpacing 0.25
        , Font.size (scaled -3)
        , Font.uppercase
        , Font.weight 700
        , Style.prop "align-self" "start"
        ]

    -----------------------------------
    -- H2
    -----------------------------------
    , style H2
        [ Font.center
        , Font.size (scaled 4)
        , Font.typeface [ headerFont, Font.sansSerif ]
        , Font.weight 700
        ]

    -----------------------------------
    -- H3
    -----------------------------------
    , style H3
        [ Font.size (scaled 4)
        , Font.typeface [ headerFont, Font.sansSerif ]
        , Font.weight 700
        ]

    -----------------------------------
    -- H4
    -----------------------------------
    , style H4
        [ Color.text colors.base06
        , Font.size (scaled -2)
        , Font.typeface [ headerFont, Font.sansSerif ]
        , Font.uppercase
        , Font.weight 500
        ]
    ]
