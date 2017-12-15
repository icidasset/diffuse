module Styles exposing (Styles(..), Variations(..), styles)

import Color exposing (..)
import Style exposing (..)
import Style.Background as Background
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Style.Sheet as Sheet
import Style.Transition as Transition
import Time
import Variables exposing (..)


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
        ]



-- Types


type Styles
    = -- 🚀
      Root
      -- Basics
    | WithoutLineHeight
      -- Buttons
    | AuthenticationButton
      -- Containers
    | AuthenticationOptions
    | Insulation
    | NestedInsulation
      -- Decorations
    | Overlay
      -- 💀
    | Zed


type Variations
    = Default



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
    style Zed
        []



-- Basics


basics : List (Style Styles Variations)
basics =
    [ -----------------------------------
      -- Without line-height
      -----------------------------------
      style WithoutLineHeight [ Font.lineHeight 0 ]
    ]



-- Buttons


buttons : List (Style Styles Variations)
buttons =
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
        , pseudo "last-child"
            [ Border.bottom 0 ]
        ]
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
    , -----------------------------------
      -- Insulation
      -----------------------------------
      style Insulation
        [ Border.rounded 3
        , Color.background white

        --
        , Shadow.box
            { offset = ( 0, 2 )
            , size = 0
            , blur = 4
            , color = rgba 0 0 0 0.2
            }
        ]
    , -----------------------------------
      -- Nested insulation
      -----------------------------------
      style NestedInsulation
        [ Border.rounded 3
        , Color.background white
        ]
    ]



-- Decorations


decorations : List (Style Styles Variations)
decorations =
    [ -----------------------------------
      -- Overlay
      -----------------------------------
      style Overlay
        [ Color.background (rgba 0 0 0 0.25)

        -- Transitions
        , { delay = 0
          , duration = Time.second * 1
          , easing = "ease"
          , props = [ "opacity" ]
          }
            |> List.singleton
            |> Transition.transitions

        --
        , opacity 0
        ]
    ]
