module Form.Styles exposing (Styles(..), styles)

import Style exposing (..)
import Style.Font as Font
import Variables exposing (scaled)
import Variations exposing (Variations)


-- ⚗️


type Styles
    = Input
    | Label
    | Select



-- 🍯


styles : List (Style Styles Variations)
styles =
    [ -----------------------------------
      -- Input
      -----------------------------------
      style Input
        []
    , -----------------------------------
      -- Label
      -----------------------------------
      style Label
        [ Font.size (scaled -3)
        , Font.uppercase
        , Font.weight 700
        ]
    , -----------------------------------
      -- Select
      -----------------------------------
      style Select
        [ Font.size (scaled -1)
        ]
    ]
