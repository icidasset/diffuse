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
                [ T.absolute
                , T.br2
                , T.bg_white
                , C.select_none
                , T.f7
                , T.overflow_hidden
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
        [ T.bb
        , T.pa3
        , T.pr4
        , T.pointer
        , T.truncate

        --
        , ifThenElse (active || isLast) T.b__transparent T.b__near_white
        , ifThenElse active C.bg_base00 ""
        , ifThenElse active T.white T.color_inherit
        , ifThenElse active T.fw6 T.fw4
        ]
        [ inline
            [ T.dib, C.leading_0, T.v_mid ]
            [ icon 14 Inherit ]
        , slab
            Html.span
            [ style "top" "-0.5px" ]
            [ T.dib, T.ml2, T.pl1, T.relative, T.v_mid ]
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
