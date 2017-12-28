module Styles exposing (Styles(..), styles)

import Color exposing (..)
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
    | SubtleButton
      -- Containers
    | AuthenticationOptions
    | EmptyState
    | Insulation
    | NestedInsulation
      -- Decorations
    | LogoBackdrop
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
        , prop "-moz-font-smoothing" "grayscale"
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
        -- Standard Button
        -----------------------------------
        , List.append
            (default)
            [ Color.border colorDerivatives.success
            , Color.text colorDerivatives.success
            ]
            |> style Button

        -----------------------------------
        -- Subtle Button
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
        , Style.prop "background-position" "-124px 53.75% !important"

        --
        , Background.imageWith
            { src = "/images/icon-dark.svg"
            , position = ( 0, 0 )
            , repeat = Background.space
            , size = Background.cover
            }
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
