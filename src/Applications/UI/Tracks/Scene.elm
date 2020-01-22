module UI.Tracks.Scene exposing (..)

import Chunky exposing (..)
import Conditional exposing (..)
import Css.Classes as C
import Html exposing (Html, text)
import Material.Icons as Icons
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
    chunk
        [ C.font_display
        , C.font_semibold
        , C.leading_normal
        , C.pb_3
        , C.px_4
        , C.text_base04
        , C.text_xxs
        , C.tracking_tad_further
        , C.truncate

        --
        , ifThenElse (0 == index) C.pt_3 C.pt_4
        ]
        [ groupIcon
        , inline [ C.align_middle ] [ text groupName ]
        ]



-- ㊙️


groupIcon : Html msg
groupIcon =
    inline
        [ C.align_middle, C.inline_block, C.leading_0, C.pr_2 ]
        [ Icons.library_music 16 Inherit ]
