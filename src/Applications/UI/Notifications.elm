module UI.Notifications exposing (view)

import Chunky exposing (..)
import Color.Ext as Color
import Css
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Notifications exposing (..)
import Tachyons.Classes as T
import UI.Core exposing (Msg)
import UI.Kit



-- 🗺


view : List (Notification Msg) -> Html Msg
view notifications =
    brick
        [ css containerStyles ]
        [ T.absolute
        , T.bottom_0
        , T.f6
        , T.lh_title
        , T.mb3
        , T.mr3
        , T.right_0
        , T.z_9999
        ]
        (List.map
            (\notification ->
                brick
                    [ css errorStyles ]
                    [ T.br2
                    , T.fw6
                    , T.measure_narrow
                    , T.mt2
                    , T.pa3
                    , T.white_90
                    ]
                    [ contents notification
                    , if .sticky (options notification) then
                        chunk
                            [ T.f7, T.i, T.mt2, T.o_60 ]
                            [ Html.text "Double click to dismiss" ]

                      else
                        nothing
                    ]
            )
            (List.reverse notifications)
        )



-- 🖼


containerStyles : List Css.Style
containerStyles =
    [ Css.lineHeight (Css.num 1.35)
    , Css.Global.descendants
        [ Css.Global.p
            [ Css.margin Css.zero
            , Css.padding Css.zero
            ]
        , Css.Global.strong
            [ Css.borderBottom3 (Css.px 1) Css.solid (Css.rgba 255 255 255 0.45)
            , Css.fontWeight Css.inherit
            ]
        ]
    ]


errorStyles : List Css.Style
errorStyles =
    [ Css.backgroundColor (Color.toElmCssColor UI.Kit.colors.error) ]
