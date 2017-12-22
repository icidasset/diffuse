module Style.Sheet.Ext exposing (..)

import Style exposing (Style)
import Style.Sheet as Sheet
import Variations exposing (Variations)


mixChild : (child -> parent) -> List (Style child Variations) -> Style parent Variations
mixChild class styles =
    styles
        |> Sheet.map class identity
        |> Sheet.merge
