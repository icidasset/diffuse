module Form.Styles exposing (Styles(..), styles)

import Style exposing (..)
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
        []
    , -----------------------------------
      -- Select
      -----------------------------------
      style Select
        []
    ]
