module UI.Tracks.Scene.List exposing (scrollToNowPlaying, scrollToTop, view)

import Browser.Dom as Dom
import Chunky exposing (..)
import Color
import Color.Ext as Color
import Conditional exposing (ifThenElse)
import Css
import Html as UnstyledHtml
import Html.Attributes as UnstyledHtmlAttributes
import Html.Events.Extra.Mouse as Mouse exposing (onWithOptions)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes exposing (css, fromUnstyled, id)
import Html.Styled.Events exposing (onClick, onDoubleClick)
import Html.Styled.Lazy
import InfiniteList
import Material.Icons.Navigation as Icons
import Tachyons.Classes as T
import Task
import Tracks exposing (..)
import UI.Kit
import UI.Reply
import UI.Tracks.Core exposing (..)



-- 🗺


type alias Necessities =
    { height : Float
    }


view : Necessities -> Model -> Html Msg
view necessities model =
    let
        { infiniteList } =
            model
    in
    brick
        [ fromUnstyled (InfiniteList.onScroll InfiniteListMsg)
        , id containerId
        ]
        [ T.flex_grow_1
        , T.vh_25
        , T.overflow_x_hidden
        , T.overflow_y_scroll
        ]
        [ Html.Styled.Lazy.lazy2 header model.sortBy model.sortDirection
        , Html.fromUnstyled
            (InfiniteList.view
                (infiniteListConfig necessities model)
                infiniteList
                model.collection.harvested
            )
        ]


containerId : String
containerId =
    "diffuse__track-list"


scrollToNowPlaying : IdentifiedTrack -> Cmd Msg
scrollToNowPlaying ( identifiers, track ) =
    (22 - rowHeight / 2 + 5 + toFloat identifiers.indexInList * rowHeight)
        |> Dom.setViewportOf containerId 0
        |> Task.attempt (always Bypass)


scrollToTop : Cmd Msg
scrollToTop =
    Task.attempt (always Bypass) (Dom.setViewportOf containerId 0 0)



-- HEADERS


header : SortBy -> SortDirection -> Html Msg
header sortBy sortDirection =
    let
        color =
            Color.rgb255 207 207 207

        sortIcon =
            (if sortDirection == Desc then
                Icons.expand_less

             else
                Icons.expand_more
            )
                color
                15

        sortIconHtml =
            Html.fromUnstyled sortIcon

        maybeSortIcon s =
            ifThenElse (sortBy == s) (Just sortIconHtml) Nothing
    in
    brick
        [ css headerStyles ]
        [ T.bg_white, T.flex, T.fw6, T.relative, T.z_5 ]
        [ headerColumn "" 4.5 First Nothing Bypass
        , headerColumn "Title" 37.5 Between (maybeSortIcon Title) (SortBy Title)
        , headerColumn "Artist" 29.0 Between (maybeSortIcon Artist) (SortBy Artist)
        , headerColumn "Album" 29.0 Last (maybeSortIcon Album) (SortBy Album)
        ]


headerStyles : List Css.Style
headerStyles =
    [ Css.borderBottom3 (Css.px 1) Css.solid (Color.toElmCssColor UI.Kit.colors.subtleBorder)
    , Css.color (Color.toElmCssColor headerTextColor)
    , Css.fontSize (Css.px 11)
    ]


headerTextColor : Color.Color
headerTextColor =
    Color.rgb255 207 207 207



-- HEADER COLUMN


type Pos
    = First
    | Between
    | Last


headerColumn :
    String
    -> Float
    -> Pos
    -> Maybe (Html msg)
    -> msg
    -> Html msg
headerColumn text_ width pos maybeSortIcon msg =
    let
        paddingLeft =
            ifThenElse (pos == First) T.pl2 T.pl1

        paddingRight =
            ifThenElse (pos == Last) T.pr2 T.pr1
    in
    brick
        [ onClick msg
        , css
            [ Css.borderLeft3
                (Css.px <| ifThenElse (pos /= First) 1 0)
                Css.solid
                (Color.toElmCssColor UI.Kit.colors.subtleBorder)
            , Css.width (Css.pct width)
            ]
        ]
        [ T.lh_title
        , T.pv1
        , T.relative

        --
        , ifThenElse (pos == First) T.pl3 T.pl2
        , ifThenElse (pos == Last) T.pr3 T.pr2
        , ifThenElse (pos == First) "" T.pointer
        ]
        [ brick
            [ css [ Css.top (Css.px 1) ] ]
            [ T.relative ]
            [ text text_ ]
        , case maybeSortIcon of
            Just sortIcon ->
                brick
                    [ css sortIconStyles ]
                    [ T.absolute, T.mr1, T.right_0 ]
                    [ sortIcon ]

            Nothing ->
                nothing
        ]


sortIconStyles : List Css.Style
sortIconStyles =
    [ Css.fontSize (Css.px 0)
    , Css.lineHeight (Css.px 0)
    , Css.top (Css.pct 50)
    , Css.transform (Css.translateY <| Css.pct -50)
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

        color =
            if isNowPlaying then
                Css.rgb 255 255 255

            else
                Color.toElmCssColor UI.Kit.colors.text
    in
    [ Css.backgroundColor bgColor
    , Css.color color
    ]



-- COLUMNS


favouriteColumn : Bool -> Identifiers -> Html Msg
favouriteColumn favouritesOnly identifiers =
    brick
        [ css (favouriteColumnStyles favouritesOnly identifiers)
        , onClick (ToggleFavourite identifiers.indexInList)
        ]
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


infiniteListConfig : Necessities -> Model -> InfiniteList.Config IdentifiedTrack Msg
infiniteListConfig necessities model =
    InfiniteList.withCustomContainer
        infiniteListContainer
        (InfiniteList.config
            { itemView = itemView model
            , itemHeight = InfiniteList.withConstantHeight (round rowHeight)
            , containerHeight = round necessities.height
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


itemView : Model -> Int -> Int -> IdentifiedTrack -> UnstyledHtml.Html Msg
itemView { favouritesOnly } _ idx ( identifiers, track ) =
    Html.toUnstyled <|
        slab
            Html.li
            [ css (rowStyles idx identifiers)

            -- Play
            -------
            , [ UI.Reply.PlayTrack ( identifiers, track ) ]
                |> Reply
                |> onDoubleClick

            -- Context Menu
            ---------------
            , ( identifiers, track )
                |> ShowContextMenu
                |> onWithOptions
                    "contextmenu"
                    { stopPropagation = True
                    , preventDefault = True
                    }
                |> Html.Styled.Attributes.fromUnstyled
            ]
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
