module Lens.Ext exposing (..)

import Monocle.Lens as Lens exposing (Lens)


adjust : Lens a b -> a -> (b -> b) -> a
adjust lens a fn =
    Lens.modify lens fn a
