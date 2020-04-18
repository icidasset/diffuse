module UI.Alfred.View exposing (view)

import Alfred exposing (Alfred)
import Chunky exposing (..)
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (autofocus, id, placeholder, type_)
import Html.Events exposing (onInput)
import Html.Ext exposing (onTapPreventDefault)
import Json.Decode
import List.Extra as List
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
import String.Ext as String
import UI.Types as UI



-- 🗺


view : Maybe (Alfred UI.Msg) -> Html UI.Msg
view maybeInstance =
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
                    , C.max_w_md
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
                , chunk
                    [ C.bg_white
                    , C.rounded
                    , C.text_sm
                    , C.leading_none
                    , C.max_w_md
                    , C.mt_8
                    , C.overflow_hidden
                    , C.shadow_md
                    , C.w_full

                    -- Dark mode
                    ------------
                    , C.dark__bg_base00
                    ]
                    (List.indexedMap
                        (\idx result ->
                            brick
                                [ onTapPreventDefault (UI.SelectAlfredItem idx) ]
                                [ C.p_4
                                , C.relative
                                , C.truncate

                                --
                                , if idx == instance.focus then
                                    String.joinWithSpace [ C.text_white, C.dark__text_base07 ]

                                  else
                                    C.text_inherit

                                --
                                , if idx == instance.focus then
                                    C.bg_accent

                                  else if modBy 2 idx == 0 then
                                    C.bg_transparent

                                  else
                                    String.joinWithSpace [ C.bg_gray_100, C.dark__bg_base01 ]
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
