module Equalizer.Styles exposing (Styles(..), styles, knobColor, knobOpacity, knobSize)

import Color
import Color.Ext as Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Variables exposing (colors, scaled)
import Variations exposing (Variations)


-- ⚗️


type Styles
    = Column
    | Knob
    | KnobLabel
    | LayerA
    | LayerB
    | Line
    | Wrapper



-- Variables


borderColor : Color.Color
borderColor =
    Color.rgba 0 0 0 0.075


knobColor : Color.Color
knobColor =
    colors.base03


knobOpacity : Float
knobOpacity =
    0.7


knobSize : Float
knobSize =
    36



-- 🍯


styles : List (Style Styles Variations)
styles =
    [ -----------------------------------
      -- Column
      -----------------------------------
      style Column
        [ Border.right 1
        , Color.border borderColor

        --
        , pseudo "last-child" [ Border.right 0 ]
        ]
    , -----------------------------------
      -- Knob
      -----------------------------------
      style Knob
        [ Border.rounded (knobSize / 2)
        , Style.cursor "pointer"

        --
        , Shadow.inset
            { offset = ( 0, 0 )
            , size = 1
            , blur = 5
            , color = Color.setAlpha (knobOpacity - 0.35) knobColor
            }
        ]
    , -----------------------------------
      -- Knob label
      -----------------------------------
      style KnobLabel
        [ Color.text (Color.rgba 0 0 0 0.4)
        , Font.center
        , Font.letterSpacing 0.25
        , Font.size 8.5
        , Font.uppercase
        , Font.weight 700
        ]
    , -----------------------------------
      -- Layers
      -----------------------------------
      style LayerA
        [ prop "border-radius" "50%"

        --
        , Shadow.box
            { offset = ( 0, 0 )
            , size = 1
            , blur = 6
            , color = Color.setAlpha (knobOpacity + 0.3) knobColor
            }
        ]
    , style LayerB
        [ knobColor
            |> Color.setAlpha (knobOpacity + 0.1)
            |> Color.background
        ]
    , -----------------------------------
      -- Line
      -----------------------------------
      style Line
        [ knobColor
            |> Color.setAlpha (knobOpacity + 0.1)
            |> Color.background
        ]
    , -----------------------------------
      -- Wrapper
      -----------------------------------
      style Wrapper
        [ Border.all 1
        , Color.border borderColor
        ]
    ]
