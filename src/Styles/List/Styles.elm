module List.Styles exposing (Styles(..), styles)

import Color
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
        , Style.cursor "pointer"

        --
        , variation Selected [ Color.text colors.base08 ]
        ]
    ]
