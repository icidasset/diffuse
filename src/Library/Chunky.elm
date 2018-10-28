module Chunky exposing (block, chunk, empty, raw, slab)

{-| Chunks, blocks and slabs.

Convenience functions to build UIs with Tachyons.

-}

import Html exposing (Html)
import Tachyons



-- 1


slab :
    (List (Html.Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Html.Attribute msg)
    -> List String
    -> List (Html msg)
    -> Html msg
slab typ attributes classes children =
    typ (List.append [ Tachyons.classes classes ] attributes) children



-- 2


block : List (Html.Attribute msg) -> List String -> List (Html msg) -> Html msg
block =
    slab Html.div



-- 3


chunk : List String -> List (Html msg) -> Html msg
chunk =
    block []



-- 4


raw : List (Html msg) -> Html msg
raw =
    chunk []



-- 5


empty : Html msg
empty =
    Html.text ""
