module UI.Tracks.Scene.Covers exposing (containerId, scrollToNowPlaying, scrollToTop, view)

import Browser.Dom as Dom
import Chunky exposing (..)
import Color exposing (Color)
import Color.Ext as Color
import Color.Manipulate as Color
import Conditional exposing (ifThenElse)
import Coordinates
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (class, id, style, tabindex)
import Html.Events
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
    { harvest : List IdentifiedTrack
    , infiniteList : InfiniteList.Model
    , isVisible : Bool
    , sortBy : SortBy
    , viewportHeight : Float
    , viewportWidth : Float
    }



-- TODO: Make use of Html.Lazy


view : Dependencies -> Html Msg
view deps =
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
        [ infiniteListView deps
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



-- INFINITE LIST


infiniteListView : Dependencies -> Html Msg
infiniteListView deps =
    let
        viewportWidth =
            round deps.viewportWidth

        tagFn =
            case deps.sortBy of
                Artist ->
                    Tuple.second >> .tags >> .artist

                Album ->
                    Tuple.second >> .tags >> .album

                PlaylistIndex ->
                    always ""

                Title ->
                    Tuple.second >> .tags >> .title

        covers =
            deps.harvest
                |> List.groupWhile (\a b -> tagFn a == tagFn b)
                |> List.map (Tuple.mapFirst tagFn)
    in
    { itemView =
        itemView

    --
    , itemHeight = InfiniteList.withVariableHeight (dynamicItemHeight viewportWidth)
    , containerHeight = round deps.viewportHeight
    }
        |> InfiniteList.config
        |> InfiniteList.withCustomContainer infiniteListContainer
        |> (\config ->
                InfiniteList.view
                    config
                    deps.infiniteList
                    covers
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
    [ C.flex
    , C.flex_wrap
    , C.pl_4
    , C.pt_5
    ]
        |> String.join " "
        |> class
        |> List.singleton


dynamicItemHeight : Int -> Int -> ( String, List IdentifiedTrack ) -> Int
dynamicItemHeight viewportWidth _ cover =
    let
        containerWidth =
            -- TODO: replace 32 with actual horizontal padding
            min 768 (viewportWidth - 32)

        rowHeight =
            (containerWidth - 16) // 5

        itemHeight =
            rowHeight // 5
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
    itemHeight + itemHeight // 2



-- ITEMS


itemView _ idx identifiedTrack =
    chunk
        [ C.h_0
        , C.overflow_hidden
        , C.pt_1_div_5
        , C.relative
        , C.w_1_div_5
        ]
        [ chunk
            [ C.absolute
            , C.bg_gray_300
            , C.inset_0
            , C.mb_4
            , C.mr_4
            , C.rounded_md

            -- Dark mode
            ------------
            , C.dark__bg_white
            , C.dark__opacity_025
            ]
            []
        ]
