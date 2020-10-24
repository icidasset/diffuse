module UI.Tracks.Scene.Covers exposing (containerId, scrollToNowPlaying, scrollToTop, view)

import Browser.Dom as Dom
import Chunky exposing (..)
import Color exposing (Color)
import Conditional exposing (ifThenElse)
import Coordinates
import Css.Classes as C
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes as A exposing (id, style, tabindex)
import Html.Events as E
import Html.Events.Extra.Mouse as Mouse
import Html.Lazy
import InfiniteList
import List.Ext as List
import List.Extra as List
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Queue
import Task
import Tracks exposing (..)
import UI.Queue.Types as Queue
import UI.Tracks.Scene as Scene
import UI.Tracks.Scene.List
import UI.Tracks.Types exposing (Msg(..))
import UI.Types as UI exposing (Msg(..))



-- 🗺


type alias Dependencies =
    { bgColor : Maybe Color
    , cachedCovers : Maybe (Dict String String)
    , covers : List Cover
    , darkMode : Bool
    , favouritesOnly : Bool
    , infiniteList : InfiniteList.Model
    , isVisible : Bool
    , nowPlaying : Maybe Queue.Item
    , selectedCover : Maybe Cover
    , selectedTrackIndexes : List Int
    , sortBy : SortBy
    , sortDirection : SortDirection
    , viewportHeight : Float
    , viewportWidth : Float
    }


type alias ItemDependencies =
    { cachedCovers : Maybe (Dict String String)
    , columns : Int
    , containerWidth : Int
    , nowPlaying : Maybe Queue.Item
    , sortBy : SortBy
    }


view : Dependencies -> Html Msg
view deps =
    Html.Lazy.lazy view_ deps


view_ : Dependencies -> Html Msg
view_ deps =
    Html.div
        [ C.flex
        , C.flex_basis_0
        , C.flex_col
        , C.flex_grow
        , C.relative
        ]
        [ collectionView deps
        , case deps.selectedCover of
            Just cover ->
                singleCoverView cover deps

            Nothing ->
                nothing
        ]



-- 🏞  ░░  COLLECTION


collectionView : Dependencies -> Html Msg
collectionView deps =
    let
        amountOfCovers =
            List.length deps.covers
    in
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
        , C.text_almost_sm
        ]
        [ Scene.shadow
        , Html.div
            [ C.bg_white
            , C.flex
            , C.items_center
            , C.pt_5
            , C.px_5
            , C.relative
            , C.z_20

            -- Dark mode
            ------------
            , C.dark__bg_darkest_hour
            ]
            [ sortGroupButtons deps.sortBy

            --
            , Html.div
                [ C.flex
                , C.flex_auto
                , C.items_center
                , C.justify_end
                , C.text_base05
                , C.text_right
                , C.text_xs
                ]
                [ text (String.fromInt amountOfCovers)
                , case deps.sortBy of
                    Album ->
                        text " albums"

                    Artist ->
                        text " artists"

                    _ ->
                        nothing
                , text " "
                , slab
                    Html.span
                    [ deps.sortBy
                        |> SortBy
                        |> TracksMsg
                        |> E.onClick

                    --
                    , case deps.sortDirection of
                        Asc ->
                            A.title "Sorted alphabetically ascending"

                        Desc ->
                            A.title "Sorted alphabetically descending"
                    ]
                    [ C.cursor_pointer
                    , C.ml_1
                    , C.opacity_60
                    ]
                    [ case deps.sortDirection of
                        Asc ->
                            Icons.arrow_downward 16 Inherit

                        Desc ->
                            Icons.arrow_upward 16 Inherit
                    ]
                ]
            ]

        --
        , infiniteListView deps
        ]


containerId : String
containerId =
    "diffuse__track-covers"


scrollToNowPlaying : Float -> List Cover -> IdentifiedTrack -> Cmd Msg
scrollToNowPlaying viewportWidth covers nowPlaying =
    let
        columns =
            determineColumns viewportWidth

        containerWidth =
            determineContainerWidth viewportWidth

        rowHeightArgs =
            { columns = columns
            , containerWidth = containerWidth
            }

        { rows, nowPlayingRowIndex } =
            coverRows (Just nowPlaying) columns covers
    in
    case nowPlayingRowIndex of
        Just idx ->
            rows
                |> List.take idx
                |> List.foldl (\a -> (+) <| dynamicRowHeight rowHeightArgs 0 a) 0
                |> toFloat
                |> (+) 11
                |> Dom.setViewportOf containerId 0
                |> Task.attempt (always Bypass)

        Nothing ->
            Cmd.none


scrollToTop : Cmd Msg
scrollToTop =
    Task.attempt (always UI.Bypass) (Dom.setViewportOf containerId 0 0)


viewAttributes : List (Html.Attribute Msg)
viewAttributes =
    [ InfiniteList.onScroll (InfiniteListMsg >> TracksMsg)
    , id containerId
    ]



-- 🏞  ░░  SINGLE COVER


singleCoverView : Cover -> Dependencies -> Html Msg
singleCoverView cover deps =
    let
        derivedColors =
            UI.Tracks.Scene.List.deriveColors
                { bgColor = deps.bgColor
                , darkMode = deps.darkMode
                }

        columns =
            determineColumns deps.viewportWidth

        condensedView =
            columns < 4
    in
    brick
        [ tabindex (ifThenElse deps.isVisible 0 -1) ]
        [ C.absolute
        , C.bg_white
        , C.flex_basis_0
        , C.flex_grow
        , C.inset_0
        , C.leading_tight
        , C.outline_none
        , C.overflow_x_hidden
        , C.overflow_y_auto
        , C.text_almost_sm
        , C.z_30

        -- Dark mode
        ------------
        , C.dark__bg_darkest_hour
        ]
        [ Html.div
            [ C.flex
            , C.font_semibold
            , C.h_8
            , C.items_center
            , C.leading_none
            , C.neg_ml_2
            , C.mt_5
            , C.px_5
            ]
            [ headerButton
                [ E.onClick (TracksMsg DeselectCover) ]
                { active = False
                , label = Icons.arrow_back 16 Inherit
                }

            --
            , headerButton
                [ Mouse.onClick (showCoverMenu cover) ]
                { active = True
                , label = Icons.more_horiz 16 Inherit
                }
            ]

        --
        , Html.div
            [ C.mb_6
            , C.neg_top_px
            , C.mt_4
            , C.relative

            --
            , ifThenElse condensedView C.block C.flex
            , ifThenElse condensedView C.mx_5 C.ml_5
            ]
            [ itemView
                { clickable = False, horizontal = condensedView }
                (compileItemDependencies deps)
                cover

            --
            , cover.tracks
                |> List.indexedMap
                    (UI.Tracks.Scene.List.defaultItemView
                        { derivedColors = derivedColors
                        , favouritesOnly = deps.favouritesOnly
                        , nowPlaying = deps.nowPlaying
                        , roundedCorners = True
                        , selectedTrackIndexes = deps.selectedTrackIndexes
                        , showAlbum = not cover.sameAlbum
                        , showArtist = deps.sortBy /= Artist && not cover.sameArtist
                        , showGroup = False
                        }
                        0
                    )
                |> Html.div
                    [ ifThenElse condensedView C.px_px C.px_0 ]
                |> List.singleton
                |> Html.div
                    [ C.flex_auto
                    , C.flex_basis_0
                    , C.overflow_hidden
                    , C.select_none

                    --
                    , ifThenElse condensedView C.neg_mx_5 C.mx_5
                    , ifThenElse condensedView C.px_1 C.px_0
                    ]
            ]
        ]



-- 🧕


headerButton attributes { active, label } =
    brick
        attributes
        [ C.cursor_pointer
        , C.inline_flex
        , C.h_8
        , C.items_center
        , C.overflow_hidden
        , C.px_2
        , C.rounded

        --
        , ifThenElse active C.bg_gray_300 C.bg_transparent
        , ifThenElse active C.dark__bg_base01 C.dark__bg_transparent
        ]
        [ Html.div
            [ C.mt_px, C.pt_px ]
            [ label ]
        ]


showCoverMenu : Cover -> Mouse.Event -> Msg
showCoverMenu cover =
    .clientPos
        >> Coordinates.fromTuple
        >> (TracksMsg << ShowCoverMenuWithSmallDelay cover)



-- SORTING


sortGroupButtons : SortBy -> Html Msg
sortGroupButtons sortBy =
    Html.div
        [ C.flex
        , C.h_8
        , C.items_center
        , C.leading_none
        , C.mr_3
        , C.text_xs
        , C.tracking_tad_further
        ]
        [ sortGroupButton
            { current = sortBy, btn = Artist }
            (Html.div
                [ C.inline_flex, C.items_center ]
                [ inline [ C.mr_px ] [ Icons.people_alt 16 Inherit ]
                , inline [ C.ml_1, C.mt_px, C.pl_px, C.pt_px ] [ text "Artists" ]
                ]
            )

        --
        , sortGroupButton
            { current = sortBy, btn = Album }
            (Html.div
                [ C.inline_flex, C.items_center ]
                [ inline [ C.mr_px ] [ Icons.album 16 Inherit ]
                , inline [ C.ml_1, C.mt_px, C.pt_px ] [ text "Albums" ]
                ]
            )
        ]


sortGroupButton : { current : SortBy, btn : SortBy } -> Html Msg -> Html Msg
sortGroupButton { current, btn } label =
    headerButton
        [ btn
            |> SortBy
            |> TracksMsg
            |> E.onClick

        --
        , C.mr_1
        ]
        { active = current == btn
        , label = label
        }



-- INFINITE LIST


infiniteListView : Dependencies -> Html Msg
infiniteListView deps =
    let
        itemDeps =
            compileItemDependencies deps

        rowHeightArgs =
            { columns = itemDeps.columns
            , containerWidth = itemDeps.containerWidth
            }
    in
    { itemView = rowView itemDeps
    , itemHeight = InfiniteList.withVariableHeight (dynamicRowHeight rowHeightArgs)
    , containerHeight = round deps.viewportHeight - 262
    }
        |> InfiniteList.config
        |> InfiniteList.withCustomContainer infiniteListContainer
        |> (\config ->
                InfiniteList.view
                    config
                    deps.infiniteList
                    (deps.covers
                        |> coverRows Nothing itemDeps.columns
                        |> .rows
                    )
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


compileItemDependencies : Dependencies -> ItemDependencies
compileItemDependencies deps =
    { cachedCovers = deps.cachedCovers
    , columns = determineColumns deps.viewportWidth
    , containerWidth = determineContainerWidth deps.viewportWidth
    , nowPlaying = deps.nowPlaying
    , sortBy = deps.sortBy
    }


listStyles : List (Html.Attribute msg)
listStyles =
    [ C.leading_tight
    , C.pl_5
    , C.pt_4
    ]



-- ROWS


determineContainerWidth : Float -> Int
determineContainerWidth viewportWidth =
    min 768 (round viewportWidth - 32)


dynamicRowHeight : { columns : Int, containerWidth : Int } -> Int -> List Cover -> Int
dynamicRowHeight { columns, containerWidth } _ coverRow =
    let
        rowHeight =
            (containerWidth - 16) // columns + (46 + 16)
    in
    let
        shouldRenderGroup =
            coverRow
                |> List.head
                |> Maybe.andThen (.tracks >> List.head)
                |> Maybe.map (Tuple.first >> Tracks.shouldRenderGroup)
                |> Maybe.withDefault False
    in
    if shouldRenderGroup then
        42 + rowHeight

    else
        rowHeight


coverRows :
    Maybe IdentifiedTrack
    -> Int
    -> List Cover
    -> { nowPlayingRowIndex : Maybe Int, rows : List (List Cover) }
coverRows maybeNowPlaying columns covers =
    covers
        |> List.foldl
            (\cover { collection, current, nowPlayingRowIdx, trackGroup } ->
                let
                    trackGroupCurr =
                        cover.identifiedTrackCover
                            |> Tuple.first
                            |> .group
                            |> Maybe.map .name

                    npr addition =
                        case ( maybeNowPlaying, nowPlayingRowIdx ) of
                            ( Just ( _, t ), Nothing ) ->
                                if List.member t.id cover.trackIds then
                                    Just (List.length collection + ifThenElse addition 1 0)

                                else
                                    Nothing

                            _ ->
                                nowPlayingRowIdx
                in
                if List.length current < columns && (Maybe.isNothing trackGroup || trackGroupCurr == trackGroup) then
                    { collection = collection
                    , current = current ++ [ cover ]
                    , nowPlayingRowIdx = npr False
                    , trackGroup = trackGroupCurr
                    }

                else
                    { collection = collection ++ [ current ]
                    , current = [ cover ]
                    , nowPlayingRowIdx = npr True
                    , trackGroup = trackGroupCurr
                    }
            )
            { current = []
            , collection = []
            , nowPlayingRowIdx = Nothing
            , trackGroup = Nothing
            }
        |> (\foldResult ->
                { nowPlayingRowIndex = foldResult.nowPlayingRowIdx
                , rows = foldResult.collection ++ [ foldResult.current ]
                }
           )


rowView :
    ItemDependencies
    -> Int
    -> Int
    -> List Cover
    -> Html Msg
rowView itemDeps _ idx row =
    let
        maybeIdentifiers =
            row
                |> List.head
                |> Maybe.andThen (.tracks >> List.head)
                |> Maybe.map Tuple.first

        shouldRenderGroup =
            maybeIdentifiers
                |> Maybe.map Tracks.shouldRenderGroup
                |> Maybe.withDefault False
    in
    raw
        [ case ( shouldRenderGroup, maybeIdentifiers ) of
            ( True, Just identifiers ) ->
                Html.div
                    [ C.neg_ml_4 ]
                    [ Scene.group { index = idx } identifiers ]

            _ ->
                nothing

        --
        , Html.div
            [ C.flex, C.flex_wrap ]
            (List.map (itemView { clickable = True, horizontal = False } itemDeps) row)
        ]



-- ITEMS / COLUMNS


determineColumns : Float -> Int
determineColumns viewportWidth =
    let
        containerWidth =
            determineContainerWidth viewportWidth
    in
    if containerWidth < 260 then
        1

    else if containerWidth < 480 then
        2

    else if containerWidth <= 590 then
        3

    else
        4


type alias ItemViewOptions =
    { clickable : Bool, horizontal : Bool }


itemView : ItemViewOptions -> ItemDependencies -> Cover -> Html Msg
itemView options deps cover =
    Html.div
        [ C.antialiased
        , C.flex_shrink_0
        , C.font_semibold

        --
        , ifThenElse options.horizontal C.flex C.block
        , ifThenElse options.horizontal C.mb_0 C.mb_5

        --
        , case ( options.horizontal, deps.columns ) of
            ( True, _ ) ->
                C.w_auto

            ( False, 1 ) ->
                C.w_full

            ( False, 2 ) ->
                C.w_1over2

            ( False, 3 ) ->
                C.w_1over3

            _ ->
                C.w_1over4
        ]
        [ coverView options deps cover
        , metadataView options deps cover
        ]


coverView : ItemViewOptions -> ItemDependencies -> Cover -> Html Msg
coverView { clickable, horizontal } { cachedCovers, nowPlaying } cover =
    let
        nowPlayingId =
            Maybe.unwrap "" (.identifiedTrack >> Tuple.second >> .id) nowPlaying

        album =
            cover.identifiedTrackCover
                |> Tuple.second
                |> .tags
                |> .album

        missingTracks =
            album == Tracks.missingAlbumPlaceholder

        maybeBlobUrlFromCache =
            cachedCovers
                |> Maybe.withDefault Dict.empty
                |> Dict.get cover.key

        hasBackgroundImage =
            Maybe.isJust maybeBlobUrlFromCache && not missingTracks

        bgOrDataAttributes =
            case ( missingTracks, maybeBlobUrlFromCache ) of
                ( True, _ ) ->
                    []

                ( False, Just blobUrl ) ->
                    [ A.style "background-image" ("url('" ++ blobUrl ++ "')")
                    ]

                ( False, Nothing ) ->
                    if Maybe.isJust cachedCovers then
                        let
                            ( identifiers, track ) =
                                cover.identifiedTrackCover
                        in
                        [ A.attribute "data-key" cover.key
                        , A.attribute "data-focus" cover.focus
                        , A.attribute "data-filename" identifiers.filename
                        , A.attribute "data-path" track.path
                        , A.attribute "data-source-id" track.sourceId
                        , A.attribute "data-various-artists" (ifThenElse cover.variousArtists "t" "f")
                        ]

                    else
                        []
    in
    Html.div
        [ C.flex_shrink_0
        , C.mr_5
        , C.relative

        --
        , ifThenElse clickable C.cursor_pointer C.cursor_default
        , ifThenElse horizontal C.h_32 C.h_0
        , ifThenElse horizontal C.mb_4 C.pt_full
        , ifThenElse horizontal C.w_32 C.w_auto
        ]
        [ brick
            (List.append
                bgOrDataAttributes
                (if clickable then
                    [ E.onClick (TracksMsg <| SelectCover cover)
                    , Mouse.onContextMenu (showCoverMenu cover)
                    ]

                 else
                    []
                )
            )
            [ C.absolute
            , C.bg_cover
            , C.bg_gray_300
            , C.mb_5
            , C.inset_0
            , C.rounded_md
            , C.shadow

            --
            , ifThenElse horizontal C.h_32 C.h_auto

            -- Dark mode
            ------------
            , C.dark__bg_white_025
            ]
            [ if not hasBackgroundImage then
                Html.div
                    [ C.absolute
                    , C.left_1over2
                    , C.neg_translate_x_1over2
                    , C.neg_translate_y_1over2
                    , C.text_gray_400
                    , C.top_1over2
                    , C.transform

                    -- Dark mode
                    ------------
                    , C.dark__text_base01
                    ]
                    [ Icons.album 26 Inherit ]

              else
                nothing

            -- Now playing?
            , if List.member nowPlayingId cover.trackIds then
                let
                    dropShadow =
                        "drop-shadow(hsla(0, 0%, 0%, 0.275) 0px 0px 2.5px)"
                in
                brick
                    [ style "-webkit-filter" dropShadow
                    , style "filter" dropShadow
                    ]
                    [ C.absolute
                    , C.bottom_0
                    , C.mb_3
                    , C.mr_3
                    , C.right_0
                    , C.text_white
                    ]
                    [ Icons.headset 16 Inherit ]

              else
                nothing
            ]
        ]


metadataView : ItemViewOptions -> ItemDependencies -> Cover -> Html Msg
metadataView { clickable, horizontal } { cachedCovers, sortBy } cover =
    let
        { identifiedTrackCover } =
            cover

        ( _, track ) =
            identifiedTrackCover

        missingTracks =
            track.tags.album == Tracks.missingAlbumPlaceholder
    in
    brick
        (if clickable then
            [ E.onClick (TracksMsg <| SelectCover cover)
            , Mouse.onContextMenu (showCoverMenu cover)
            ]

         else
            []
        )
        [ C.mr_5
        , C.relative
        , C.tracking_tad_closer
        , C.z_10

        --
        , ifThenElse clickable C.cursor_pointer C.cursor_default
        , ifThenElse horizontal C.mt_0 C.neg_mt_5
        , ifThenElse horizontal C.overflow_hidden C.overflow_auto
        , ifThenElse horizontal C.pt_0 C.pt_2
        ]
        [ Html.div
            [ C.mt_px
            , C.pt_px
            , C.truncate
            ]
            [ case sortBy of
                Album ->
                    text track.tags.album

                Artist ->
                    text track.tags.artist

                _ ->
                    nothing
            ]

        --
        , Html.div
            [ C.mt_px
            , C.pt_px
            , C.text_base05
            , C.text_xs
            , C.truncate
            ]
            [ case sortBy of
                Album ->
                    if missingTracks then
                        text "Missing tracks"

                    else if cover.variousArtists then
                        text "Various Artists"

                    else
                        text track.tags.artist

                Artist ->
                    case List.length cover.trackIds of
                        1 ->
                            Html.text "1 track"

                        n ->
                            Html.text (String.fromInt n ++ " tracks")

                _ ->
                    nothing
            ]
        ]
