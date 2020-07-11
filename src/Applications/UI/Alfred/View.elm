module UI.Alfred.View exposing (view)

import Alfred exposing (Alfred)
import Chunky exposing (..)
import Color exposing (Color)
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (autofocus, id, placeholder, style, type_)
import Html.Events exposing (onInput)
import Html.Ext exposing (onTapPreventDefault)
import Json.Decode
import List.Extra as List
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import String.Ext as String
import UI.Types as UI



-- 🗺


view : Maybe (Alfred UI.Msg) -> Maybe Color -> Html UI.Msg
view maybeInstance extractedBackdropColor =
    let
        bgColor =
            Maybe.unwrap "inherit" Color.toCssString extractedBackdropColor
    in
    case maybeInstance of
        Just instance ->
            chunk
                [ C.inset_0
                , C.flex
                , C.flex_col
                , C.fixed
                , C.items_center
                , C.px_3
                , C.cursor_pointer
                , C.z_50
                ]
                [ -----------------------------------------
                  -- Message
                  -----------------------------------------
                  chunk
                    [ C.italic
                    , C.leading_normal
                    , C.mt_12
                    , C.text_center
                    , C.text_white

                    -- Dark mode
                    ------------
                    , C.dark__text_base07
                    ]
                    [ text instance.message ]

                -----------------------------------------
                -- Search
                -----------------------------------------
                , brick
                    [ Html.Events.custom
                        "tap"
                        (Json.Decode.succeed
                            { message = UI.Bypass
                            , stopPropagation = True
                            , preventDefault = True
                            }
                        )
                    ]
                    [ C.text_sm
                    , C.max_w_lg
                    , C.mt_8
                    , C.w_full
                    ]
                    [ slab
                        Html.input
                        [ autofocus True
                        , id "diffuse__alfred"
                        , onInput UI.GotAlfredInput
                        , placeholder "Type to search or create"
                        , type_ "text"
                        ]
                        [ C.border_none
                        , C.bg_white
                        , C.block
                        , C.leading_normal
                        , C.rounded
                        , C.outline_none
                        , C.p_4
                        , C.shadow_md
                        , C.text_2xl
                        , C.tracking_tad_closer
                        , C.w_full

                        -- Dark mode
                        ------------
                        , C.dark__bg_base00
                        ]
                        []
                    ]

                -----------------------------------------
                -- Results
                -----------------------------------------
                , brick
                    [ id "alfred__results" ]
                    [ C.bg_white
                    , C.rounded
                    , C.leading_none
                    , C.max_w_lg
                    , C.mb_32
                    , C.mt_8
                    , C.overflow_x_hidden
                    , C.overflow_y_auto
                    , C.shadow_md
                    , C.smooth_scrolling
                    , C.text_nearly_sm
                    , C.w_full

                    -- Dark mode
                    ------------
                    , C.dark__bg_base00
                    ]
                    (List.indexedMap
                        (\idx result ->
                            brick
                                [ onTapPreventDefault (UI.SelectAlfredItem idx)

                                --
                                , if idx == instance.focus then
                                    id "alfred__results__focus"

                                  else
                                    id ("alfred__results__" ++ String.fromInt idx)

                                --
                                , if idx == instance.focus then
                                    style "background-color" bgColor

                                  else
                                    style "" ""
                                ]
                                [ C.p_4
                                , C.relative
                                , C.truncate

                                --
                                , if idx == instance.focus then
                                    String.joinWithSpace [ C.text_white, C.dark__text_base07 ]

                                  else
                                    C.text_inherit

                                --
                                , if modBy 2 idx == 0 then
                                    ""

                                  else
                                    String.joinWithSpace [ C.bg_gray_100, C.dark__bg_base01_15 ]
                                ]
                                [ text result

                                --
                                , if idx == instance.focus then
                                    chunk
                                        [ C.absolute
                                        , C.leading_0
                                        , C.minus_translate_y_half
                                        , C.mr_3
                                        , C.right_0
                                        , C.top_half
                                        , C.transform
                                        ]
                                        [ Icons.keyboard_return 13 Inherit
                                        ]

                                  else
                                    nothing
                                ]
                        )
                        instance.results
                    )
                ]

        Nothing ->
            nothing
