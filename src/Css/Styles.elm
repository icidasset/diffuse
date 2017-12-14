module Styles exposing (Styles(..), Variations(..), styles)

import Color exposing (..)
import Style exposing (..)
import Style.Background as Background
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Sheet as Sheet
import Variables exposing (..)


-- 🍯


styles : StyleSheet Styles Variations
styles =
    Style.styleSheet
        [ root

        --
        , Sheet.mix containers
        ]



-- Types


type Styles
    = -- 🚀
      Root
      -- Containers
    | Insulation
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
        , Font.typeface [ Font.sansSerif ]

        --
        , prop "-webkit-font-smoothing" "antialiased"
        , prop "-moz-font-smoothing" "grayscale"
        , prop "font-smooth" "always"
        ]



-- Containers


containers : List (Style Styles Variations)
containers =
    [ -----------------------------------
      -- Insulation
      -----------------------------------
      style Insulation
        [ Color.background Color.white
        ]
    ]
