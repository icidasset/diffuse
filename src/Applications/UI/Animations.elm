module UI.Animations exposing (fadeIn)

import Css exposing (num)
import Css.Animations exposing (..)



-- 🖼


fadeIn : Keyframes {}
fadeIn =
    keyframes
        [ ( 0, [ opacity (num 0) ] )
        , ( 100, [ opacity (num 1) ] )
        ]
