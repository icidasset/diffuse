module UI.ContextMenu exposing (view)

import Chunky exposing (..)
import Conditional exposing (..)
import ContextMenu exposing (..)
import Coordinates exposing (Coordinates)
import Css
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (custom)
import Json.Decode
import Material.Icons exposing (Coloring(..))
import Tachyons.Classes as T
import UI.Kit
import UI.Reply exposing (Reply)



-- 🗺


view : Maybe (ContextMenu Reply) -> Html Reply
view m =
    case m of
        Just (ContextMenu items coordinates) ->
            brick
                -- TODO: [ css (menuStyles coordinates) ]
                []
                [ C.absolute
                , C.rounded
                , C.bg_white
                , C.select_none
                , C.text_xs
                , C.overflow_hidden
                , T.z_9999
                ]
                (let
                    lastIndex =
                        List.length items - 1
                 in
                 List.indexedMap
                    (\idx item ->
                        case item of
                            Item i ->
                                itemView lastIndex idx i

                            Divider ->
                                -- NOTE: Not needed at the moment
                                nothing
                    )
                    items
                )

        Nothing ->
            nothing


itemView : Int -> Int -> ContextMenu.ItemProperties Reply -> Html Reply
itemView lastIndex index { icon, label, msg, active } =
    let
        isLast =
            index == lastIndex
    in
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
        , T.pa3
        , C.pr_4
        , C.cursor_pointer
        , C.truncate

        --
        , ifThenElse (active || isLast) T.b__transparent T.b__near_white
        , ifThenElse active C.bg_base00 ""
        , ifThenElse active C.text_white T.color_inherit
        , ifThenElse active C.font_semibold T.fw4
        ]
        [ inline
            [ C.inline_block, C.leading_0, C.align_middle ]
            [ icon 14 Inherit ]
        , slab
            Html.span
            [ style "top" "-0.5px" ]
            [ C.inline_block, C.ml_2, C.pl_1, C.relative, C.align_middle ]
            [ text label ]
        ]



-- 🖼


menuStyles : Coordinates -> List Css.Style
menuStyles { x, y } =
    [ Css.fontSize (Css.px 12.5)
    , Css.left (Css.px x)
    , Css.minWidth (Css.px 170)
    , Css.transform (Css.translate2 (Css.pct -50) (Css.pct -50))
    , Css.top (Css.px y)

    -- TODO: , UI.Kit.onOverlayShadow
    ]
