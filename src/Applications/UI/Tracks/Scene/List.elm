module UI.Tracks.Scene.List exposing (view)

import Chunky exposing (..)
import Color.Ext as Color
import Conditional exposing (ifThenElse)
import Css
import Html as UnstyledHtml
import Html.Attributes as UnstyledHtmlAttributes
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes exposing (css, fromUnstyled)
import InfiniteList
import Tachyons.Classes as T
import Tracks exposing (..)
import UI.Kit



-- 🗺


type alias Necessities msg =
    { favouritesOnly : Bool
    , infiniteList : InfiniteList.Model
    , screenHeight : Float
    , scrollMsg : InfiniteList.Model -> msg
    }


view : Necessities msg -> List IdentifiedTrack -> Html msg
view necessities tracks =
    let
        { favouritesOnly, infiniteList, scrollMsg } =
            necessities
    in
    brick
        [ fromUnstyled (InfiniteList.onScroll scrollMsg) ]
        [ T.flex_grow_1
        , T.vh_25
        , T.overflow_x_hidden
        , T.overflow_y_scroll
        ]
        [ Html.fromUnstyled
            (InfiniteList.view
                (infiniteListConfig necessities)
                infiniteList
                tracks
            )
        ]



-- ROWS


rowHeight : Float
rowHeight =
    35


rowStyles : Int -> Identifiers -> List Css.Style
rowStyles idx { isNowPlaying } =
    let
        bgColor =
            if isNowPlaying then
                Color.toElmCssColor UI.Kit.colorKit.base0D

            else if modBy 2 idx == 1 then
                Css.rgb 252 252 252

            else
                Css.rgb 255 255 255
    in
    [ Css.backgroundColor bgColor
    ]



-- COLUMNS


favouriteColumn : Bool -> Identifiers -> Html msg
favouriteColumn favouritesOnly identifiers =
    brick
        [ css (favouriteColumnStyles favouritesOnly identifiers) ]
        [ T.dtc, T.pl3, T.v_mid ]
        [ if identifiers.isFavourite then
            text "t"

          else
            text "f"
        ]


favouriteColumnStyles : Bool -> Identifiers -> List Css.Style
favouriteColumnStyles favouritesOnly { isFavourite, isNowPlaying, isSelected } =
    let
        color =
            if isSelected then
                Color.toElmCssColor UI.Kit.colors.selection

            else if isNowPlaying && isFavourite then
                Css.rgb 255 255 255

            else if isNowPlaying then
                Css.rgba 255 255 255 0.4

            else if favouritesOnly || not isFavourite then
                Css.rgb 222 222 222

            else
                Color.toElmCssColor UI.Kit.colorKit.base08
    in
    [ Css.color color
    , Css.fontFamilies [ "or-favourites" ]
    , Css.height (Css.px rowHeight)
    , Css.width (Css.pct 4.5)
    ]


otherColumn : Float -> Bool -> String -> Html msg
otherColumn width isLast text_ =
    brick
        [ css (otherColumnStyles width) ]
        [ T.dtc
        , T.pl2
        , T.truncate
        , T.v_mid

        --
        , ifThenElse isLast T.pr3 T.pr2
        ]
        [ text text_ ]


otherColumnStyles : Float -> List Css.Style
otherColumnStyles columnWidth =
    [ Css.height (Css.px rowHeight)
    , Css.width (Css.pct columnWidth)
    ]



-- INFINITE LIST


infiniteListConfig : Necessities msg -> InfiniteList.Config IdentifiedTrack msg
infiniteListConfig necessities =
    InfiniteList.withCustomContainer
        infiniteListContainer
        (InfiniteList.config
            { itemView = itemView necessities
            , itemHeight = InfiniteList.withConstantHeight (round rowHeight)
            , containerHeight = round necessities.screenHeight
            }
        )


infiniteListContainer :
    List ( String, String )
    -> List (UnstyledHtml.Html msg)
    -> UnstyledHtml.Html msg
infiniteListContainer styles children =
    UnstyledHtml.div
        (List.map (\( k, v ) -> UnstyledHtmlAttributes.style k v) styles)
        [ (Html.toUnstyled << rawy) <|
            slab
                Html.ol
                [ css listStyles ]
                [ T.dt
                , T.dt__fixed
                , T.f6
                , T.list
                , T.ma0
                , T.ph0
                , T.pv1
                ]
                (List.map Html.fromUnstyled children)
        ]


listStyles : List Css.Style
listStyles =
    [ Css.fontSize (Css.px 12.5)
    ]


itemView : Necessities msg -> Int -> Int -> IdentifiedTrack -> UnstyledHtml.Html msg
itemView { favouritesOnly } _ idx ( identifiers, track ) =
    Html.toUnstyled <|
        slab
            Html.li
            [ css (rowStyles idx identifiers) ]
            [ T.dt_row

            --
            , ifThenElse identifiers.isMissing "" T.pointer
            , ifThenElse identifiers.isSelected T.fw6 ""
            ]
            [ favouriteColumn favouritesOnly identifiers
            , otherColumn 37.5 False track.tags.title
            , otherColumn 29.0 False track.tags.artist
            , otherColumn 29.0 True track.tags.album
            ]
