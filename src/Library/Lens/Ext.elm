module Lens.Ext exposing (..)

import Monocle.Lens as Lens exposing (Lens)


{-| Flipped version of `Lens.modify`.
-}
adjust : Lens a b -> a -> (b -> b) -> a
adjust lens a fn =
    Lens.modify lens fn a


{-| Flipped version of `lens.set`.
-}
replace : Lens a b -> a -> b -> a
replace lens a b =
    lens.set b a
