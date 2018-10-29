module UI.Navigation exposing (global, globalItem)

import Chunky exposing (..)
import Color
import Conditional exposing (..)
import Html exposing (Html, text)
import Html.Attributes exposing (href, style)
import Tachyons.Classes as T
import UI.Core exposing (Msg)
import UI.Kit
import UI.Page as Page exposing (Page)



-- Global


global : List ( Page, String ) -> Page -> Html Msg
global items activePage =
    block
        [ style "font-size" "11.5px" ]
        [ T.f7
        , T.mb5
        , T.mt4
        , T.tracked
        , T.ttu
        ]
        (List.indexedMap (globalItem activePage <| List.length items) items)


globalItem : Page -> Int -> Int -> ( Page, String ) -> Html Msg
globalItem activePage totalItems idx ( page, label ) =
    let
        isActivePage =
            page == activePage

        isLastItem =
            idx + 1 == totalItems
    in
    slab
        Html.a
        [ href (Page.toString page)

        --
        , style "color" (ifThenElse isActivePage globalActiveColor globalDefaultColor)
        , style "border-bottom-color" (ifThenElse isActivePage globalBorderColor "transparent")
        ]
        [ T.bb
        , T.dib
        , T.lh_copy
        , T.no_underline
        , T.pt2

        --
        , ifThenElse isLastItem T.mr0 T.mr4
        ]
        [ text label ]


{-| TODO - Wait for avh4/elm-color v1.1.0
-}
globalActiveColor =
    "rgb(65, 50, 63)"


globalDefaultColor =
    "rgba(65, 50, 63, 0.725)"


globalBorderColor =
    "rgba(65, 50, 63, 0.125)"
