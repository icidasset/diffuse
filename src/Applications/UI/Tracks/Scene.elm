module UI.Tracks.Scene exposing (..)

import Conditional exposing (..)
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes as A
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
    Html.div
        [ A.style "height" "18px"

        --
        , C.box_content
        , C.font_display
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
        , Html.span [ C.align_middle ] [ text groupName ]
        ]


shadow : Html msg
shadow =
    Html.div
        [ C.h_10
        , C.left_0
        , C.neg_mt_10
        , C.neg_translate_y_full
        , C.opacity_30
        , C.right_0
        , C.shadow_md
        , C.sticky
        , C.top_0
        , C.transform
        , C.z_10

        -- Dark mode
        ------------
        , C.dark__shadow_md_darker
        ]
        []



-- ㊙️


groupIcon : Html msg
groupIcon =
    Html.span
        [ C.align_middle, C.inline_block, C.leading_0, C.pr_2 ]
        [ Icons.library_music 16 Inherit ]
