module UI.Tracks.Scene.Covers exposing (containerId, scrollToNowPlaying, scrollToTop, view)

import Browser.Dom as Dom
import Chunky exposing (..)
import Color exposing (Color)
import Color.Ext as Color
import Color.Manipulate as Color
import Conditional exposing (ifThenElse)
import Coordinates
import Css.Classes as C
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes as A exposing (class, id, style, tabindex)
import Html.Events as E
import Html.Events.Extra.Mouse as Mouse
import Html.Lazy
import InfiniteList
import Json.Decode as Decode
import List.Ext as List
import List.Extra as List
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Queue
import Task
import Tracks exposing (..)
import UI.DnD as DnD
import UI.Kit
import UI.Queue.Types as Queue
import UI.Tracks.Scene as Scene
import UI.Tracks.Types exposing (Msg(..))
import UI.Types as UI exposing (Msg(..))



-- 🗺


type alias Dependencies =
    { cachedCovers : Maybe (Dict String String)
    , covers : List Cover
    , infiniteList : InfiniteList.Model
    , isVisible : Bool
    , sortBy : SortBy
    , viewportHeight : Float
    , viewportWidth : Float
    }


type alias ItemDependencies =
    { cachedCovers : Maybe (Dict String String)
    }


view : Dependencies -> Html Msg
view deps =
    Html.Lazy.lazy
        view_
        deps


view_ : Dependencies -> Html Msg
view_ deps =
    brick
        ((::)
            (tabindex (ifThenElse deps.isVisible 0 -1))
            viewAttributes
        )
        [ C.flex_basis_0
        , C.flex_grow
        , C.outline_none
        , C.overflow_x_hidden
        , C.overflow_y_auto
        , C.relative
        , C.scrolling_touch
        ]
        [ sortGroupButtons deps.sortBy
        , infiniteListView deps
        ]


containerId : String
containerId =
    "diffuse__track-covers"


scrollToNowPlaying : List IdentifiedTrack -> IdentifiedTrack -> Cmd Msg
scrollToNowPlaying harvest ( identifiers, _ ) =
    Cmd.none


scrollToTop : Cmd Msg
scrollToTop =
    Task.attempt (always UI.Bypass) (Dom.setViewportOf containerId 0 0)


viewAttributes : List (Html.Attribute Msg)
viewAttributes =
    [ InfiniteList.onScroll (InfiniteListMsg >> TracksMsg)
    , id containerId
    ]



-- SORTING


sortGroupButtons : SortBy -> Html Msg
sortGroupButtons sortBy =
    chunk
        [ C.antialiased
        , C.flex
        , C.font_semibold
        , C.leading_none
        , C.pt_5
        , C.px_4
        , C.text_xs
        , C.tracking_wide
        ]
        [ sortGroupButton { current = sortBy, btn = Artist } "Artists"
        , sortGroupButton { current = sortBy, btn = Album } "Albums"
        ]


sortGroupButton : { current : SortBy, btn : SortBy } -> String -> Html Msg
sortGroupButton { current, btn } label =
    let
        active =
            current == btn
    in
    brick
        [ btn
            |> SortBy
            |> TracksMsg
            |> E.onClick
        ]
        [ C.p_2
        , C.rounded

        --
        , ifThenElse active C.bg_gray_300 C.bg_transparent
        , ifThenElse active C.dark__bg_base01 C.dark__bg_transparent
        , ifThenElse active C.cursor_default C.cursor_pointer
        ]
        [ chunk
            [ C.pb_px
            , C.pt_1
            , C.px_px
            ]
            [ text label ]
        ]



-- INFINITE LIST


infiniteListView : Dependencies -> Html Msg
infiniteListView deps =
    let
        itemDeps =
            { cachedCovers = deps.cachedCovers }

        viewportWidth =
            round deps.viewportWidth

        coverGroups =
            List.greedyGroupsOf 5 deps.covers
    in
    { itemView = rowView itemDeps
    , itemHeight = InfiniteList.withVariableHeight (dynamicItemHeight viewportWidth)
    , containerHeight = round deps.viewportHeight - 262
    }
        |> InfiniteList.config
        |> InfiniteList.withCustomContainer infiniteListContainer
        |> (\config ->
                InfiniteList.view
                    config
                    deps.infiniteList
                    coverGroups
           )


infiniteListContainer :
    List ( String, String )
    -> List (Html msg)
    -> Html msg
infiniteListContainer styles =
    styles
        |> List.filterMap
            (\( k, v ) ->
                if k == "padding" then
                    Nothing

                else
                    Just (style k v)
            )
        |> List.append listStyles
        |> Html.div


listStyles : List (Html.Attribute msg)
listStyles =
    [ C.pl_4
    , C.pt_4
    ]
        |> String.join " "
        |> class
        |> List.singleton


dynamicItemHeight viewportWidth _ coverGroup =
    let
        containerWidth =
            -- TODO: replace 32 with actual horizontal padding
            min 768 (viewportWidth - 32)

        rowHeight =
            (containerWidth - 16) // 5
    in
    -- TODO
    -- let
    --     shouldRenderGroup =
    --         i.group
    --             |> Maybe.map (.firstInGroup >> (==) True)
    --             |> Maybe.withDefault False
    -- in
    -- if shouldRenderGroup then
    --     32 + 18 + 16 + rowHeight
    --
    -- else
    -- itemHeight + itemHeight // 2
    rowHeight



-- ROWS


rowView :
    ItemDependencies
    -> Int
    -> Int
    -> List Cover
    -> Html Msg
rowView itemDeps _ idx row =
    chunk
        [ C.flex, C.flex_wrap ]
        (List.map (itemView itemDeps) row)



-- ITEMS


itemView : ItemDependencies -> Cover -> Html Msg
itemView { cachedCovers } cover =
    let
        maybeBlobUrlFromCache =
            cachedCovers
                |> Maybe.withDefault Dict.empty
                |> Dict.get cover.key
    in
    brick
        [ cover.identifiedTrack
            |> Queue.InjectFirstAndPlay
            |> QueueMsg
            |> Decode.succeed
            |> E.on "dbltap"
        ]
        [ C.cursor_pointer
        , C.h_0
        , C.overflow_hidden
        , C.pt_1_div_5
        , C.relative
        , C.select_none
        , C.w_1_div_5
        ]
        [ brick
            (case maybeBlobUrlFromCache of
                Just blobUrl ->
                    [ A.style
                        "background-image"
                        ("url('" ++ blobUrl ++ "')")
                    ]

                Nothing ->
                    if Maybe.isJust cachedCovers then
                        let
                            track =
                                Tuple.second cover.identifiedTrack
                        in
                        [ A.attribute "data-key" cover.key
                        , A.attribute "data-focus" cover.focus
                        , A.attribute "data-filename" cover.trackFilename
                        , A.attribute "data-path" track.path
                        , A.attribute "data-source-id" track.sourceId
                        ]

                    else
                        []
            )
            [ C.absolute
            , C.bg_cover
            , C.bg_gray_300
            , C.inset_0
            , C.mb_4
            , C.mr_4
            , C.rounded_md

            -- Dark mode
            ------------
            , C.dark__bg_white_025
            ]
            []
        ]
