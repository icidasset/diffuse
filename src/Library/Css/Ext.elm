module Css.Ext exposing (disableUserSelection)

import Css



-- 🔱


disableUserSelection : Css.Style
disableUserSelection =
    Css.batch
        [ Css.property "-webkit-user-select" "none"
        , Css.property "-moz-user-select" "none"
        , Css.property "-ms-user-select" "none"
        , Css.property "user-select" "none"
        ]
