module Chunky.Styled exposing (brick, bricky, chunk, chunky, inline, lineBreak, nothing, raw, rawy, slab, slaby)

{-| Chunks, blocks and slabs.

Convenience functions to build UIs with composable CSS classes.

-}

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (class)



-- 1


slab :
    (List (Html.Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Html.Attribute msg)
    -> List String
    -> List (Html msg)
    -> Html msg
slab typ attributes classes children =
    typ
        (List.append
            (if List.isEmpty classes then
                []

             else
                [ classes
                    |> String.join " "
                    |> class
                ]
            )
            attributes
        )
        children


slaby :
    (List (Html.Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Html.Attribute msg)
    -> List String
    -> Html msg
    -> Html msg
slaby a b c =
    List.singleton >> slab a b c



-- 2


brick : List (Html.Attribute msg) -> List String -> List (Html msg) -> Html msg
brick =
    slab Html.div


bricky : List (Html.Attribute msg) -> List String -> Html msg -> Html msg
bricky a b =
    List.singleton >> brick a b



-- 3


chunk : List String -> List (Html msg) -> Html msg
chunk =
    brick []


chunky : List String -> Html msg -> Html msg
chunky a =
    List.singleton >> chunk a


inline : List String -> List (Html msg) -> Html msg
inline =
    slab Html.span []



-- 4


raw : List (Html msg) -> Html msg
raw =
    chunk []


rawy : Html msg -> Html msg
rawy =
    List.singleton >> raw



-- 5


nothing : Html msg
nothing =
    Html.text ""


lineBreak : Html msg
lineBreak =
    Html.br [] []
