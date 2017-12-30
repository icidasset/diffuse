module Tracks.Styles exposing (Styles(..), styles, iconColor, trackHeight)

import Color exposing (Color)
import Color.Ext as Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Variables exposing (..)
import Variations exposing (Variations)


-- ⚗️


type Styles
    = ClickableAction
    | Navigation
    | Placeholder
    | Search
    | Table
    | TableHeader



-- Variables


iconColor : Color
iconColor =
    Color.rgb 205 205 205


trackHeight : Int
trackHeight =
    33



-- 🍯


styles : List (Style Styles Variations)
styles =
    [ -----------------------------------
      -- Clickable action
      -----------------------------------
      style ClickableAction
        [ Font.lineHeight 0
        , Style.cursor "pointer"
        ]

    -----------------------------------
    -- Navigation
    -----------------------------------
    , style Navigation
        [ Shadow.box
            { offset = ( 0, 0 )
            , size = 1
            , blur = 10
            , color = Color.rgba 0 0 0 0.05
            }
        ]

    -----------------------------------
    -- Placeholder
    -----------------------------------
    , style Placeholder
        [ Border.bottom 2
        , Color.border colorDerivatives.text
        , Font.size (scaled -1)
        , Font.weight 600
        ]

    -----------------------------------
    -- Search
    -----------------------------------
    , style Search
        [ Border.bottom 1
        , Border.right 1
        , Color.background (Color.rgba 0 0 0 0)
        , Color.border colorDerivatives.subtleBorder
        , Color.placeholder (Color.rgb 205 205 205)
        , Font.size 14

        --
        , focus
            [ Color.border colorDerivatives.subtleBorder
            , Style.prop "box-shadow" "none"
            ]
        ]

    -----------------------------------
    -- Table
    -----------------------------------
    , style Table
        [ Color.text colors.base02
        , Font.lineHeight ((toFloat trackHeight) / 12)
        , Font.size 12

        --
        , prop "-webkit-user-select" "none"
        , prop "-moz-user-select" "none"
        , prop "-ms-user-select" "none"
        , prop "user-select" "none"
        ]
    ]
