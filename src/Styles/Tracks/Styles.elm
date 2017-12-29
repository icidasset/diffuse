module Tracks.Styles exposing (Styles(..), styles, iconColor)

import Color exposing (Color)
import Color.Ext as Color
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Variables exposing (..)
import Variations exposing (Variations)


-- ⚗️


type Styles
    = ClickableAction
    | Search



-- Variables


iconColor : Color
iconColor =
    Color.rgb 205 205 205



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
    ]
