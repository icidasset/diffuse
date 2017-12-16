module ContextMenu.Styles exposing (Styles(..), styles)

import Color exposing (rgba)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Variables exposing (scaled)
import Variations exposing (Variations)


-- 🍯


styles : List (Style Styles Variations)
styles =
    [ -----------------------------------
      -- Container
      -----------------------------------
      style Container
        [ Color.background Color.white
        , Font.size (scaled -2)

        --
        , prop "box-shadow" "0 1px 3px 0 rgba(0, 0, 0, 0.225), 0 3px 15px 0 rgba(0, 0, 0, 0.1)"
        , prop "transform" "translate(-50%, -50%)"
        ]
    , -----------------------------------
      -- Item
      -----------------------------------
      style Item
        [ Border.bottom 1
        , Color.border (rgba 0 0 0 0.075)

        --
        , cursor "pointer"
        , pseudo "last-child" [ Border.bottom 0 ]
        ]
    ]



-- Types


type Styles
    = Container
    | Item
