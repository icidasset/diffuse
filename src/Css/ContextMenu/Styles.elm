module ContextMenu.Styles exposing (Styles(..), styles)

import Style exposing (..)
import Variations exposing (Variations)


-- 🍯


styles : List (Style Styles Variations)
styles =
    [ -----------------------------------
      -- Container
      -----------------------------------
      style Container
        []
    ]



-- Types


type Styles
    = Container
