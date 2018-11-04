module Chunky exposing (block, blocky, chunk, chunky, empty, lineBreak, raw, rawy, slab, slaby)

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


slaby :
    (List (Html.Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Html.Attribute msg)
    -> List String
    -> Html msg
    -> Html msg
slaby a b c =
    List.singleton >> slab a b c



-- 2


block : List (Html.Attribute msg) -> List String -> List (Html msg) -> Html msg
block =
    slab Html.div


blocky : List (Html.Attribute msg) -> List String -> Html msg -> Html msg
blocky a b =
    List.singleton >> block a b



-- 3


chunk : List String -> List (Html msg) -> Html msg
chunk =
    block []


chunky : List String -> Html msg -> Html msg
chunky a =
    List.singleton >> chunk a



-- 4


raw : List (Html msg) -> Html msg
raw =
    chunk []


rawy : Html msg -> Html msg
rawy =
    List.singleton >> raw



-- 5


empty : Html msg
empty =
    Html.text ""


lineBreak : Html msg
lineBreak =
    Html.br [] []
