module Themes.Sunrise.Tracks.Scene exposing (..)

import Chunky exposing (..)
import Conditional exposing (..)
import Html exposing (Html, text)
import Html.Attributes as A
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import Tracks



-- 🗺


group : { index : Int } -> Tracks.Identifiers -> Html msg
group { index } identifiers =
    let
        groupName =
            identifiers.group
                |> Maybe.map .name
                |> Maybe.withDefault "Unknown"
    in
    brick
        [ A.style "height" "18px" ]
        [ "box-content"
        , "font-display"
        , "font-semibold"
        , "leading-normal"
        , "pb-3"
        , "px-4"
        , "text-base04"
        , "text-xxs"
        , "tracking-tad-further"
        , "truncate"

        --
        , ifThenElse (0 == index) "pt-3" "pt-4"
        ]
        [ groupIcon
        , inline [ "align-middle" ] [ text groupName ]
        ]


shadow : Html msg
shadow =
    chunk
        [ "h-10"
        , "left-0"
        , "-mt-10"
        , "-translate-y-full"
        , "opacity-30"
        , "right-0"
        , "shadow-md"
        , "sticky"
        , "top-0"
        , "transform"
        , "z-10"

        -- Dark mode
        ------------
        , "dark:shadow-md-darker"
        ]
        []



-- ㊙️


groupIcon : Html msg
groupIcon =
    inline
        [ "align-middle", "inline-block", "leading-0", "pr-2" ]
        [ Icons.library_music 16 Inherit ]
