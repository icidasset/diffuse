module Themes.Sunrise.Tracks.Scene.List exposing (Dependencies, DerivedColors, containerId, defaultItemView, deriveColors, scrollToNowPlaying, scrollToTop, view)

import Browser.Dom as Dom
import Chunky exposing (..)
import Color exposing (Color)
import Color.Manipulate as Color
import Conditional exposing (ifThenElse)
import Coordinates
import Html exposing (Html, text)
import Html.Attributes exposing (class, id, style, tabindex)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Lazy
import InfiniteList
import Json.Decode as Decode
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Task
import Themes.Sunrise.Kit as Kit
import Themes.Sunrise.Tracks.Scene as Scene
import Tracks exposing (..)
import UI.DnD as DnD
import UI.Queue.Types as Queue
import UI.Tracks.Types exposing (Msg(..))
import UI.Types as UI exposing (Msg(..))



-- 🗺


type alias Dependencies =
    { bgColor : Maybe Color
    , darkMode : Bool
    , height : Float
    , isTouchDevice : Bool
    , isVisible : Bool
    , showAlbum : Bool
    }


type alias DerivedColors =
    { background : String
    , subtle : String
    , text : String
    }


view : Dependencies -> List IdentifiedTrack -> InfiniteList.Model -> Bool -> Maybe IdentifiedTrack -> Maybe String -> SortBy -> SortDirection -> List Int -> Maybe (DnD.Model Int) -> Html Msg
view deps harvest infiniteList favouritesOnly nowPlaying searchTerm sortBy sortDirection selectedTrackIndexes maybeDnD =
    brick
        (tabindex (ifThenElse deps.isVisible 0 -1) :: viewAttributes)
        [ "flex-basis-0"
        , "flex-grow"
        , "outline-none"
        , "overflow-x-hidden"
        , "relative"
        , "select-none"
        , "scrolling-touch"
        , "text-xs"

        --
        , "md:text-almost-sm"

        --
        , case maybeDnD of
            Just dnd ->
                if deps.isTouchDevice && DnD.isDragging dnd then
                    "overflow-y-hidden"

                else
                    "overflow-y-auto"

            Nothing ->
                "overflow-y-auto"
        ]
        [ Scene.shadow

        -- Header
        ---------
        , Html.Lazy.lazy4
            header
            (Maybe.isJust maybeDnD)
            deps.showAlbum
            sortBy
            sortDirection

        -- List
        -------
        , Html.Lazy.lazy7
            infiniteListView
            deps
            harvest
            infiniteList
            favouritesOnly
            searchTerm
            ( nowPlaying, selectedTrackIndexes )
            maybeDnD
        ]


containerId : String
containerId =
    "diffuse__track-list"


scrollToNowPlaying : List IdentifiedTrack -> IdentifiedTrack -> Cmd Msg
scrollToNowPlaying harvest ( identifiers, _ ) =
    harvest
        |> List.take identifiers.indexInList
        |> List.foldl (\a -> (+) <| dynamicRowHeight 0 a) 0
        |> (\n -> 22 - toFloat rowHeight / 2 + 2 + toFloat n)
        |> Dom.setViewportOf containerId 0
        |> Task.attempt (always Bypass)


scrollToTop : Cmd Msg
scrollToTop =
    Task.attempt (always UI.Bypass) (Dom.setViewportOf containerId 0 0)


viewAttributes : List (Html.Attribute Msg)
viewAttributes =
    [ InfiniteList.onScroll (InfiniteListMsg >> TracksMsg)
    , id containerId
    , class "overscroll-none"
    ]



-- HEADERS


header : Bool -> Bool -> SortBy -> SortDirection -> Html Msg
header isPlaylist showAlbum sortBy sortDirection =
    let
        sortIcon =
            (if sortDirection == Desc then
                Icons.expand_less

             else
                Icons.expand_more
            )
                15
                Inherit

        maybeSortIcon s =
            ifThenElse (sortBy == s) (Just sortIcon) Nothing
    in
    chunk
        [ "antialiased"
        , "bg-white"
        , "border-b"
        , "border-gray-300"
        , "flex"
        , "font-semibold"
        , "relative"
        , "text-base06"
        , "text-xxs"
        , "z-20"

        -- Dark mode
        ------------
        , "dark:bg-darkest-hour"
        , "dark:border-base01"
        , "dark:text-base03"
        ]
        (if isPlaylist && showAlbum then
            [ headerColumn "" 4.5 Nothing Bypass
            , headerColumn "#" 4.5 Nothing Bypass
            , headerColumn "Title" 36.0 Nothing Bypass
            , headerColumn "Artist" 27.5 Nothing Bypass
            , headerColumn "Album" 27.5 Nothing Bypass
            ]

         else if isPlaylist then
            [ headerColumn "" 4.5 Nothing Bypass
            , headerColumn "#" 4.5 Nothing Bypass
            , headerColumn "Title" 49.75 Nothing Bypass
            , headerColumn "Artist" 41.25 Nothing Bypass
            ]

         else if showAlbum then
            [ headerColumn "" 4.5 Nothing Bypass
            , headerColumn "Title" 37.5 (maybeSortIcon Title) (TracksMsg <| SortBy Title)
            , headerColumn "Artist" 29.0 (maybeSortIcon Artist) (TracksMsg <| SortBy Artist)
            , headerColumn "Album" 29.0 (maybeSortIcon Album) (TracksMsg <| SortBy Album)
            ]

         else
            [ headerColumn "" 5.75 Nothing Bypass
            , headerColumn "Title" 51.25 (maybeSortIcon Title) (TracksMsg <| SortBy Title)
            , headerColumn "Artist" 43 (maybeSortIcon Artist) (TracksMsg <| SortBy Artist)
            ]
        )



-- HEADER COLUMN


headerColumn : String -> Float -> Maybe (Html Msg) -> Msg -> Html Msg
headerColumn text_ width maybeSortIcon msg =
    brick
        [ Html.Events.onClick msg

        --
        , style "min-width" columnMinWidth
        , style "width" (String.fromFloat width ++ "%")
        ]
        [ "border-l"
        , "border-gray-300"
        , "leading-relaxed"
        , "pl-2"
        , "pr-2"
        , "pt-px"
        , "relative"

        --
        , case msg of
            Bypass ->
                "cursor-default"

            _ ->
                "cursor-pointer"

        --
        , "first:border-l-0"
        , "first:cursor-default"
        , "first:pl-4"
        , "last:pr-4"

        -- Dark mode
        ------------
        , "dark:border-base01"
        ]
        [ chunk
            [ "mt-px", "opacity-90", "pt-px" ]
            [ Html.text text_ ]
        , case maybeSortIcon of
            Just sortIcon ->
                chunk
                    [ "absolute"
                    , "-translate-y-1/2"
                    , "mr-1"
                    , "opacity-90"
                    , "right-0"
                    , "top-1/2"
                    , "transform"
                    ]
                    [ sortIcon ]

            Nothing ->
                nothing
        ]



-- INFINITE LIST


infiniteListView : Dependencies -> List IdentifiedTrack -> InfiniteList.Model -> Bool -> Maybe String -> ( Maybe IdentifiedTrack, List Int ) -> Maybe (DnD.Model Int) -> Html Msg
infiniteListView deps harvest infiniteList favouritesOnly searchTerm ( nowPlaying, selectedTrackIndexes ) maybeDnD =
    let
        derivedColors =
            deriveColors { bgColor = deps.bgColor, darkMode = deps.darkMode }
    in
    { itemView =
        case maybeDnD of
            Just dnd ->
                playlistItemView
                    favouritesOnly
                    nowPlaying
                    searchTerm
                    selectedTrackIndexes
                    dnd
                    deps.showAlbum
                    deps.darkMode
                    derivedColors

            _ ->
                defaultItemView
                    { derivedColors = derivedColors
                    , favouritesOnly = favouritesOnly
                    , nowPlaying = nowPlaying
                    , roundedCorners = False
                    , selectedTrackIndexes = selectedTrackIndexes
                    , showAlbum = deps.showAlbum
                    , showArtist = True
                    , showGroup = True
                    }

    --
    , itemHeight = InfiniteList.withVariableHeight dynamicRowHeight
    , containerHeight = round deps.height
    }
        |> InfiniteList.config
        |> InfiniteList.withCustomContainer infiniteListContainer
        |> (\config ->
                InfiniteList.view
                    config
                    infiniteList
                    harvest
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


deriveColors : { bgColor : Maybe Color, darkMode : Bool } -> DerivedColors
deriveColors { bgColor, darkMode } =
    let
        color =
            Maybe.withDefault Kit.colors.text bgColor
    in
    if darkMode then
        { background = Color.toCssString color
        , subtle = Color.toCssString (Color.darken 0.1 color)
        , text = Color.toCssString (Color.darken 0.475 color)
        }

    else
        { background = Color.toCssString (Color.fadeOut 0.625 color)
        , subtle = Color.toCssString (Color.fadeOut 0.575 color)
        , text = Color.toCssString (Color.darken 0.3 color)
        }


listStyles : List (Html.Attribute msg)
listStyles =
    [ class "pb-2 pt-1" ]


dynamicRowHeight : Int -> IdentifiedTrack -> Int
dynamicRowHeight _ ( i, t ) =
    if Tracks.shouldRenderGroup i then
        16 + 18 + 12 + rowHeight

    else
        rowHeight



-- INFINITE LIST ITEM


defaultItemView :
    { derivedColors : DerivedColors
    , favouritesOnly : Bool
    , nowPlaying : Maybe IdentifiedTrack
    , roundedCorners : Bool
    , selectedTrackIndexes : List Int
    , showAlbum : Bool
    , showArtist : Bool
    , showGroup : Bool
    }
    -> Int
    -> Int
    -> IdentifiedTrack
    -> Html Msg
defaultItemView args _ idx identifiedTrack =
    let
        { derivedColors, favouritesOnly, nowPlaying, roundedCorners, selectedTrackIndexes, showAlbum, showArtist, showGroup } =
            args

        ( identifiers, track ) =
            identifiedTrack

        shouldRenderGroup =
            showGroup && Tracks.shouldRenderGroup identifiers

        isSelected =
            List.member idx selectedTrackIndexes

        isOddRow =
            modBy 2 idx == 1

        rowIdentifiers =
            { isMissing = identifiers.isMissing
            , isNowPlaying = Maybe.unwrap False (isNowPlaying identifiedTrack) nowPlaying
            , isSelected = isSelected
            }

        favIdentifiers =
            { indexInList = identifiers.indexInList
            , isFavourite = identifiers.isFavourite
            , isNowPlaying = rowIdentifiers.isNowPlaying
            , isSelected = isSelected
            }
    in
    Html.div
        []
        [ if shouldRenderGroup then
            Scene.group { index = idx } identifiers

          else
            nothing

        --
        , brick
            (List.concat
                [ rowStyles idx rowIdentifiers derivedColors

                --
                , List.append
                    (if isSelected then
                        [ touchContextMenuEvent identifiedTrack Nothing ]

                     else
                        []
                    )
                    [ mouseContextMenuEvent identifiedTrack
                    , playEvent identifiedTrack
                    , selectEvent idx
                    ]
                ]
            )
            [ "flex"
            , "items-center"

            --
            , ifThenElse identifiers.isMissing "cursor-default" "cursor-pointer"
            , ifThenElse isSelected "font-semibold" "font-normal"
            , ifThenElse roundedCorners "rounded" "border-r-0"

            --
            , ifThenElse
                isOddRow
                "bg-white"
                "bg-gray-100"

            -- Dark mode
            ------------
            , ifThenElse
                isOddRow
                "dark:bg-darkest-hour"
                "dark:bg-near-darkest-hour"
            ]
            (if not showArtist && not showAlbum then
                [ favouriteColumn "5.75%" favouritesOnly favIdentifiers derivedColors
                , otherColumn "94.25%" False track.tags.title
                ]

             else if not showArtist && showAlbum then
                [ favouriteColumn "5.75%" favouritesOnly favIdentifiers derivedColors
                , otherColumn "51.25%" False track.tags.title
                , otherColumn "43%" False (Maybe.withDefault fallbackAlbum track.tags.album)
                ]

             else if showArtist && not showAlbum then
                [ favouriteColumn "5.75%" favouritesOnly favIdentifiers derivedColors
                , otherColumn "51.25%" False track.tags.title
                , otherColumn "43%" False (Maybe.withDefault fallbackArtist track.tags.artist)
                ]

             else
                [ favouriteColumn defFavColWidth favouritesOnly favIdentifiers derivedColors
                , otherColumn "37.5%" False track.tags.title
                , otherColumn "29.0%" False (Maybe.withDefault fallbackArtist track.tags.artist)
                , otherColumn "29.0%" True (Maybe.withDefault fallbackAlbum track.tags.album)
                ]
            )
        ]


playlistItemView : Bool -> Maybe IdentifiedTrack -> Maybe String -> List Int -> DnD.Model Int -> Bool -> Bool -> DerivedColors -> Int -> Int -> IdentifiedTrack -> Html Msg
playlistItemView favouritesOnly nowPlaying _ selectedTrackIndexes dnd showAlbum darkMode derivedColors _ idx identifiedTrack =
    let
        ( identifiers, track ) =
            identifiedTrack

        listIdx =
            identifiers.indexInList

        dragEnv =
            { model = dnd
            , toMsg = DnD
            }

        isSelected =
            List.member idx selectedTrackIndexes

        isOddRow =
            modBy 2 idx == 1

        rowIdentifiers =
            { isMissing = identifiers.isMissing
            , isNowPlaying = Maybe.unwrap False (isNowPlaying identifiedTrack) nowPlaying
            , isSelected = isSelected
            }

        favIdentifiers =
            { indexInList = identifiers.indexInList
            , isFavourite = identifiers.isFavourite
            , isNowPlaying = rowIdentifiers.isNowPlaying
            , isSelected = isSelected
            }
    in
    brick
        (List.concat
            [ rowStyles idx rowIdentifiers derivedColors

            --
            , List.append
                (if isSelected then
                    [ touchContextMenuEvent identifiedTrack (Just dragEnv)
                    , DnD.listenToStart dragEnv listIdx
                    ]

                 else
                    []
                )
                [ mouseContextMenuEvent identifiedTrack
                , playEvent identifiedTrack
                , selectEvent idx
                ]

            --
            , DnD.listenToEnterLeave dragEnv listIdx

            --
            , if DnD.isBeingDraggedOver listIdx dnd then
                [ dragIndicator darkMode ]

              else
                []
            ]
        )
        [ "flex"
        , "items-center"

        --
        , ifThenElse identifiers.isMissing "cursor-default" "cursor-pointer"
        , ifThenElse isSelected "font-semibold" "font-normal"

        --
        , ifThenElse
            isOddRow
            "bg-white"
            "bg-gray-100"

        -- Dark mode
        ------------
        , ifThenElse
            isOddRow
            "dark:bg-darkest-hour"
            "dark:bg-near-darkest-hour"
        ]
        (if showAlbum then
            [ favouriteColumn defFavColWidth favouritesOnly favIdentifiers derivedColors
            , playlistIndexColumn (Maybe.withDefault 0 identifiers.indexInPlaylist)
            , otherColumn "36.0%" False track.tags.title
            , otherColumn "27.5%" False (Maybe.withDefault fallbackArtist track.tags.artist)
            , otherColumn "27.5%" True (Maybe.withDefault fallbackAlbum track.tags.album)
            ]

         else
            [ favouriteColumn defFavColWidth favouritesOnly favIdentifiers derivedColors
            , playlistIndexColumn (Maybe.withDefault 0 identifiers.indexInPlaylist)
            , otherColumn "49.75%" False track.tags.title
            , otherColumn "41.25%" False (Maybe.withDefault fallbackArtist track.tags.artist)
            ]
        )


mouseContextMenuEvent : IdentifiedTrack -> Html.Attribute Msg
mouseContextMenuEvent ( i, _ ) =
    Html.Events.custom
        "contextmenu"
        (Decode.map
            (\event ->
                { message =
                    if event.keys.shift then
                        Bypass

                    else
                        event.clientPos
                            |> Coordinates.fromTuple
                            |> ShowTracksMenuWithSmallDelay
                                (Just i.indexInList)
                                { alt = event.keys.alt }
                            |> TracksMsg

                --
                , stopPropagation = True
                , preventDefault = True
                }
            )
            Mouse.eventDecoder
        )


touchContextMenuEvent : IdentifiedTrack -> Maybe (DnD.Environment Int Msg) -> Html.Attribute Msg
touchContextMenuEvent ( i, _ ) maybeDragEnv =
    Html.Events.custom
        "longtap"
        (Decode.map2
            (\x y ->
                { message =
                    -- Only show menu when not dragging something
                    case Maybe.andThen (.model >> DnD.modelTarget) maybeDragEnv of
                        Just _ ->
                            Bypass

                        Nothing ->
                            { x = x, y = y }
                                |> ShowTracksMenu
                                    (Just i.indexInList)
                                    { alt = False }
                                |> TracksMsg

                --
                , stopPropagation = False
                , preventDefault = False
                }
            )
            (Decode.field "x" Decode.float)
            (Decode.field "y" Decode.float)
        )


playEvent : IdentifiedTrack -> Html.Attribute Msg
playEvent ( i, t ) =
    Html.Events.custom
        "dbltap"
        (Decode.succeed
            { message =
                if i.isMissing then
                    Bypass

                else
                    ( i, t )
                        |> Queue.InjectFirstAndPlay
                        |> QueueMsg

            --
            , stopPropagation = True
            , preventDefault = True
            }
        )


selectEvent : Int -> Html.Attribute Msg
selectEvent idx =
    Html.Events.custom
        "tap"
        (Decode.map2
            (\shiftKey button ->
                { message =
                    case button of
                        0 ->
                            { shiftKey = shiftKey }
                                |> MarkAsSelected idx
                                |> TracksMsg

                        _ ->
                            Bypass

                --
                , stopPropagation = True
                , preventDefault = False
                }
            )
            (Decode.at [ "originalEvent", "shiftKey" ] Decode.bool)
            (Decode.oneOf
                [ Decode.at [ "originalEvent", "button" ] Decode.int
                , Decode.succeed 0
                ]
            )
        )



-- ROWS


rowHeight : Int
rowHeight =
    35


rowStyles : Int -> { isMissing : Bool, isNowPlaying : Bool, isSelected : Bool } -> DerivedColors -> List (Html.Attribute msg)
rowStyles _ { isMissing, isNowPlaying } derivedColors =
    let
        bgColor =
            if isNowPlaying then
                derivedColors.background

            else
                ""

        color =
            if isNowPlaying then
                derivedColors.text

            else if isMissing then
                rowFontColors.gray

            else
                ""
    in
    [ style "background-color" bgColor
    , style "color" color
    , style "height" (String.fromInt rowHeight ++ "px")
    ]



-- COLUMNS


defFavColWidth =
    "4.5%"


columnMinWidth =
    "28px"


favouriteColumn : String -> Bool -> { isFavourite : Bool, indexInList : Int, isNowPlaying : Bool, isSelected : Bool } -> DerivedColors -> Html Msg
favouriteColumn columnWidth favouritesOnly identifiers derivedColors =
    brick
        (List.append
            [ style "width" columnWidth
            , identifiers.indexInList
                |> ToggleFavourite
                |> TracksMsg
                |> Html.Events.onClick
            ]
            (favouriteColumnStyles favouritesOnly identifiers derivedColors)
        )
        [ "flex-shrink-0"
        , "font-normal"
        , "pl-4"
        , "text-gray-500"

        -- Dark mode
        ------------
        , "dark:text-base02"
        ]
        [ if identifiers.isFavourite then
            text "t"

          else
            text "f"
        ]


favouriteColumnStyles : Bool -> { isFavourite : Bool, indexInList : Int, isNowPlaying : Bool, isSelected : Bool } -> DerivedColors -> List (Html.Attribute msg)
favouriteColumnStyles favouritesOnly { isFavourite, isNowPlaying } derivedColors =
    let
        color =
            if isNowPlaying && isFavourite then
                derivedColors.text

            else if isNowPlaying then
                derivedColors.subtle

            else if favouritesOnly || not isFavourite then
                ""

            else
                favColors.red
    in
    [ style "color" color
    , style "font-family" "or-favourites"
    , style "min-width" columnMinWidth
    ]


playlistIndexColumn : Int -> Html msg
playlistIndexColumn indexInPlaylist =
    brick
        (otherColumnStyles "4.5%")
        [ "pl-2"
        , "pr-2"
        , "pointer-events-none"
        , "truncate"
        ]
        [ indexInPlaylist
            |> (+) 1
            |> String.fromInt
            |> text
        ]


otherColumn : String -> Bool -> String -> Html msg
otherColumn width _ text_ =
    brick
        (otherColumnStyles width)
        [ "pl-2"
        , "pr-2"
        , "pointer-events-none"
        , "truncate"

        --
        , "last:pr-4"
        ]
        [ text text_ ]


otherColumnStyles : String -> List (Html.Attribute msg)
otherColumnStyles columnWidth =
    [ style "min-width" columnMinWidth
    , style "width" columnWidth
    ]



-- 🖼


favColors =
    { gray = Color.toCssString (Color.rgb255 220 220 220)
    , red = Color.toCssString Kit.colorKit.base08
    }


rowFontColors =
    { gray = Color.toCssString Kit.colorKit.base04
    , white = Color.toCssString (Color.rgb 1 1 1)
    }


dragIndicator : Bool -> Html.Attribute msg
dragIndicator darkMode =
    let
        color =
            if darkMode then
                Kit.colors.gray_300

            else
                Kit.colorKit.base03
    in
    style "box-shadow" ("0 1px 0 0 " ++ Color.toCssString color ++ " inset")
