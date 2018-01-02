module List.Styles exposing (Styles(..), styles)

import Color
import Color.Convert
import String.Interpolate exposing (interpolate)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Variables exposing (colorDerivatives, colors, scaled)
import Variations exposing (Variations(..))


-- ⚗️


type Styles
    = Container
    | Item
    | Prefix



-- 🍯


styles : List (Style Styles Variations)
styles =
    [ -----------------------------------
      -- Container
      -----------------------------------
      style Container
        [ Font.size (scaled -2)
        , Font.weight 600
        ]

    -----------------------------------
    -- Item
    -----------------------------------
    , style Item
        [ Border.bottom 1
        , Color.border (Color.rgb 248 248 248)

        --
        , variation Clickable [ Style.cursor "pointer" ]
        , variation Draggable [ Style.cursor "move" ]
        , variation DraggingOver [ Style.prop "border-top" draggedBorder ]
        , variation Subtle [ Color.text colors.base05 ]
        ]

    -----------------------------------
    -- Prefix
    -----------------------------------
    , style Prefix
        [ Font.lineHeight 2
        , Font.size (scaled -3)
        ]
    ]


draggedBorder : String
draggedBorder =
    interpolate
        "1px solid {0}"
        [ Color.Convert.colorToCssRgb colors.base06 ]
