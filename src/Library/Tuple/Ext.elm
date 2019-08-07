module Tuple.Ext exposing (uncurry)

-- 🔱


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry fn ( a, b ) =
    fn a b
