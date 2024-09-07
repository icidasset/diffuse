module Themes.Sunrise.Tracks.Scene.Covers exposing (containerId, scrollToNowPlaying, scrollToTop, view)

import Browser.Dom as Dom
import Chunky exposing (..)
import Color exposing (Color)
import Conditional exposing (ifThenElse)
import Coordinates
import Dict exposing (Dict)
import Html exposing (Html, text)
import Html.Attributes as A exposing (class, id, style, tabindex)
import Html.Events as E
import Html.Events.Extra.Mouse as Mouse
import Html.Lazy
import InfiniteList
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Task
import Themes.Sunrise.Tracks.Scene as Scene
import Themes.Sunrise.Tracks.Scene.List
import Tracks exposing (..)
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
    , nowPlaying : Maybe IdentifiedTrack
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
    , nowPlaying : Maybe IdentifiedTrack
    , sortBy : SortBy
    }


view : Dependencies -> Html Msg
view deps =
    Html.Lazy.lazy view_ deps


view_ : Dependencies -> Html Msg
view_ deps =
    chunk
        [ "flex"
        , "flex-basis-0"
        , "flex-col"
        , "flex-grow"
        , "relative"
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
        (tabindex (ifThenElse deps.isVisible 0 -1) :: viewAttributes)
        [ "flex-basis-0"
        , "flex-grow"
        , "outline-none"
        , "overflow-x-hidden"
        , "overflow-y-auto"
        , "relative"
        , "scrolling-touch"
        , "text-almost-sm"
        ]
        [ Scene.shadow
        , chunk
            [ "bg-white"
            , "flex"
            , "items-center"
            , "pt-5"
            , "px-5"
            , "relative"
            , "z-20"

            -- Dark mode
            ------------
            , "dark:bg-darkest-hour"
            ]
            [ sortGroupButtons deps.sortBy

            --
            , chunk
                [ "flex"
                , "flex-auto"
                , "items-center"
                , "justify-end"
                , "text-base05"
                , "text-right"
                , "text-xs"
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
                    [ "cursor-pointer"
                    , "ml-1"
                    , "opacity-60"
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
            Themes.Sunrise.Tracks.Scene.List.deriveColors
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
        [ "absolute"
        , "bg-white"
        , "flex-basis-0"
        , "flex-grow"
        , "inset-0"
        , "leading-tight"
        , "outline-none"
        , "overflow-x-hidden"
        , "overflow-y-auto"
        , "text-almost-sm"
        , "z-30"

        -- Dark mode
        ------------
        , "dark:bg-darkest-hour"
        ]
        [ chunk
            [ "flex"
            , "font-semibold"
            , "h-8"
            , "items-center"
            , "leading-none"
            , "-ml-2"
            , "mt-5"
            , "px-5"
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
        , chunk
            [ "mb-6"
            , "-top-px"
            , "mt-4"
            , "relative"

            --
            , ifThenElse condensedView "block" "flex"
            , ifThenElse condensedView "mx-5" "ml-5"
            ]
            [ itemView
                { clickable = False, horizontal = condensedView }
                (compileItemDependencies deps)
                cover

            --
            , cover.tracks
                |> List.indexedMap
                    (Themes.Sunrise.Tracks.Scene.List.defaultItemView
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
                |> chunk
                    [ ifThenElse condensedView "px-px" "px-0" ]
                |> List.singleton
                |> chunk
                    [ "flex-auto"
                    , "flex-basis-0"
                    , "overflow-hidden"
                    , "select-none"

                    --
                    , ifThenElse condensedView "-mx-5" "mx-5"
                    , ifThenElse condensedView "px-1" "px-0"
                    ]
            ]
        ]



-- 🧕


headerButton attributes { active, label } =
    brick
        attributes
        [ "cursor-pointer"
        , "inline-flex"
        , "h-8"
        , "items-center"
        , "overflow-hidden"
        , "px-2"
        , "rounded"

        --
        , ifThenElse active "bg-gray-300" "bg-transparent"
        , ifThenElse active "dark:bg-base01" "dark:bg-transparent"
        ]
        [ chunk
            [ "mt-px", "pt-px" ]
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
    chunk
        [ "flex"
        , "h-8"
        , "items-center"
        , "leading-none"
        , "mr-3"
        , "text-xs"
        , "tracking-tad-further"
        ]
        [ sortGroupButton
            { current = sortBy, btn = Artist }
            (chunk
                [ "inline-flex", "items-center" ]
                [ inline [ "mr-px" ] [ Icons.people_alt 16 Inherit ]
                , inline [ "ml-1", "mt-px", "pl-px", "pt-px" ] [ text "Artists" ]
                ]
            )

        --
        , sortGroupButton
            { current = sortBy, btn = Album }
            (chunk
                [ "inline-flex", "items-center" ]
                [ inline [ "mr-px" ] [ Icons.album 16 Inherit ]
                , inline [ "ml-1", "mt-px", "pt-px" ] [ text "Albums" ]
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
        , class "mr-1"
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
    [ class "leading-tight"
    , class "pl-5"
    , class "pt-4"
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
                chunk
                    [ "-ml-4" ]
                    [ Scene.group { index = idx } identifiers ]

            _ ->
                nothing

        --
        , chunk
            [ "flex", "flex-wrap" ]
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
    chunk
        [ "antialiased"
        , "flex-shrink-0"
        , "font-semibold"

        --
        , ifThenElse options.horizontal "flex" "block"
        , ifThenElse options.horizontal "mb-0" "mb-5"

        --
        , case ( options.horizontal, deps.columns ) of
            ( True, _ ) ->
                "w-auto"

            ( False, 1 ) ->
                "w-full"

            ( False, 2 ) ->
                "w-1/2"

            ( False, 3 ) ->
                "w-1/3"

            _ ->
                "w-1/4"
        ]
        [ coverView options deps cover
        , metadataView options deps cover
        ]


coverView : ItemViewOptions -> ItemDependencies -> Cover -> Html Msg
coverView { clickable, horizontal } { cachedCovers, nowPlaying } cover =
    let
        nowPlayingId =
            Maybe.unwrap "" (Tuple.second >> .id) nowPlaying

        missingTracks =
            List.any
                (Tuple.first >> .isMissing)
                cover.tracks

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
                        , A.attribute "data-filename" identifiers.filename
                        , A.attribute "data-path" track.path
                        , A.attribute "data-source-id" track.sourceId
                        , A.attribute "data-various-artists" (ifThenElse cover.variousArtists "t" "f")
                        ]

                    else
                        []
    in
    chunk
        [ "flex-shrink-0"
        , "mr-5"
        , "relative"

        --
        , ifThenElse clickable "cursor-pointer" "cursor-default"
        , ifThenElse horizontal "h-32" "h-0"
        , ifThenElse horizontal "mb-4" "pt-full"
        , ifThenElse horizontal "w-32" "w-auto"
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
            [ "absolute"
            , "bg-cover"
            , "bg-gray-300"
            , "mb-5"
            , "inset-0"
            , "rounded-md"
            , "shadow"

            --
            , ifThenElse horizontal "h-32" "h-auto"

            -- Dark mode
            ------------
            , "dark:bg-white-025"
            ]
            [ if not hasBackgroundImage then
                chunk
                    [ "absolute"
                    , "left-1/2"
                    , "-translate-x-1/2"
                    , "-translate-y-1/2"
                    , "text-gray-400"
                    , "top-1/2"
                    , "transform"

                    -- Dark mode
                    ------------
                    , "dark:text-base01"
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
                    [ "absolute"
                    , "bottom-0"
                    , "mb-3"
                    , "mr-3"
                    , "right-0"
                    , "text-white"
                    ]
                    [ Icons.headset 16 Inherit ]

              else
                nothing
            ]
        ]


metadataView : ItemViewOptions -> ItemDependencies -> Cover -> Html Msg
metadataView { clickable, horizontal } { sortBy } cover =
    let
        { identifiedTrackCover } =
            cover

        ( _, track ) =
            identifiedTrackCover

        missingTracks =
            List.any
                (Tuple.first >> .isMissing)
                cover.tracks
    in
    brick
        (if clickable then
            [ E.onClick (TracksMsg <| SelectCover cover)
            , Mouse.onContextMenu (showCoverMenu cover)
            ]

         else
            []
        )
        [ "mr-5"
        , "relative"
        , "tracking-tad-closer"
        , "z-10"

        --
        , ifThenElse clickable "cursor-pointer" "cursor-default"
        , ifThenElse horizontal "mt-0" "-mt-5"
        , ifThenElse horizontal "overflow-hidden" "overflow-auto"
        , ifThenElse horizontal "pt-0" "pt-2"
        ]
        [ chunk
            [ "mt-px"
            , "pt-px"
            , "truncate"
            ]
            [ case sortBy of
                Album ->
                    if missingTracks then
                        text "Missing tracks"

                    else
                        text (Maybe.withDefault "Unknown album" track.tags.album)

                Artist ->
                    if missingTracks then
                        text "Missing tracks"

                    else
                        text (Maybe.withDefault "Unknown artist" track.tags.artist)

                _ ->
                    nothing
            ]

        --
        , chunk
            [ "mt-px"
            , "pt-px"
            , "text-base05"
            , "text-xs"
            , "truncate"
            ]
            [ case sortBy of
                Album ->
                    if cover.variousArtists then
                        text "Various Artists"

                    else if not missingTracks && Maybe.isJust track.tags.artist then
                        text (Maybe.withDefault "" track.tags.artist)

                    else
                        case List.length cover.trackIds of
                            1 ->
                                text "1 track"

                            n ->
                                text (String.fromInt n ++ " tracks")

                Artist ->
                    case List.length cover.trackIds of
                        1 ->
                            text "1 track"

                        n ->
                            text (String.fromInt n ++ " tracks")

                _ ->
                    nothing
            ]
        ]
