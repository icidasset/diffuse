module UI.ContextMenu exposing (view)

import Chunky exposing (..)
import Conditional exposing (..)
import ContextMenu exposing (..)
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (custom)
import Json.Decode
import Material.Icons exposing (Coloring(..))
import UI.Reply exposing (Reply)



-- 🗺


view : Maybe (ContextMenu Reply) -> Html Reply
view m =
    case m of
        Just (ContextMenu items coordinates) ->
            brick
                [ style "left" (String.fromFloat coordinates.x ++ "px")
                , style "top" (String.fromFloat coordinates.y ++ "px")

                --
                , style "min-width" "170px"
                ]
                [ C.absolute
                , C.bg_white
                , C.leading_loose
                , C.overflow_hidden
                , C.rounded
                , C.shadow_md
                , C.select_none
                , C.text_almost_sm
                , C.translate_centered
                , C.z_50
                ]
                (List.map
                    (\item ->
                        case item of
                            Item i ->
                                itemView i

                            Divider ->
                                -- NOTE: Not needed at the moment
                                nothing
                    )
                    items
                )

        Nothing ->
            nothing


itemView : ContextMenu.ItemProperties Reply -> Html Reply
itemView { icon, label, msg, active } =
    brick
        [ custom
            "tap"
            (Json.Decode.succeed
                { message = UI.Reply.ReplyViaContextMenu msg
                , stopPropagation = True
                , preventDefault = True
                }
            )
        ]
        [ C.border_b
        , C.border_very_subtle
        , C.cursor_pointer
        , C.pl_4
        , C.pr_8
        , C.py_3
        , C.truncate

        --
        , C.last__border_transparent

        --
        , ifThenElse active C.antialiased ""
        , ifThenElse active C.border_transparent ""
        , ifThenElse active C.bg_base00 ""
        , ifThenElse active C.text_white C.text_inherit
        , ifThenElse active C.font_semibold C.font_normal
        ]
        [ inline
            [ C.align_middle
            , C.inline_block
            , C.leading_0
            ]
            [ icon 14 Inherit ]
        , inline
            [ C.align_middle
            , C.inline_block
            , C.ml_2
            , C.pl_1
            , C.relative
            ]
            [ text label ]
        ]
