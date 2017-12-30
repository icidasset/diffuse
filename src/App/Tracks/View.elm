module Tracks.View exposing (entry)

import Color
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Mouse
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Playlists.Types exposing (Playlist)
import Playlists.Utils exposing (..)
import Queue.Types
import Routing.Types
import Sources.Types exposing (IsProcessing, Source)
import Tracks.Types exposing (..)
import Types as TopLevel exposing (Msg(..))


-- Elements

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Ext exposing (..)
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Types exposing (..)
import Layouts exposing (..)
import Variables exposing (colorDerivatives, colors, scaled, scaledStr)
import Variations exposing (Variations(..))


-- Html

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Lazy


-- Icons

import Material.Icons.Action
import Material.Icons.Av
import Material.Icons.Content
import Material.Icons.Editor
import Material.Icons.Image
import Material.Icons.Navigation


-- Styles

import Styles exposing (Styles(..))
import Tracks.Styles exposing (Styles(..), iconColor, trackHeight)


-- 🍯


entry : TopLevel.Model -> Node
entry model =
    column
        Zed
        [ height fill ]
        [ lazy3
            navigation
            model.tracks.searchTerm
            model.tracks.favouritesOnly
            model.tracks.selectedPlaylist
        , -- TODO: Use Element.Lazy once it's available
          content
            model.tracks.collection.exposed
            ( model.tracks.sortBy
            , model.tracks.sortDirection
            , model.tracks.selectedPlaylist
            , model.tracks.favouritesOnly
            , model.sources.isProcessing
            , model.sources.collection
            )
        ]



-- Views


navigation : Maybe String -> Bool -> Maybe Playlist -> Node
navigation searchTerm favouritesOnly maybeSelectedPlaylist =
    row
        (Tracks Tracks.Styles.Navigation)
        []
        [ -----------------------------------
          -- Part 1
          -----------------------------------
          within
            [ -- Search icon
              --
              row
                Zed
                [ height fill
                , spacing (scaled -8)
                , verticalCenter
                ]
                [ el
                    WithoutLineHeight
                    [ moveRight 2, paddingLeft (scaled -3) ]
                    (16
                        |> Material.Icons.Action.search iconColor
                        |> html
                    )
                ]

            -- Other icons
            --
            , row
                Zed
                [ alignRight
                , height fill
                , spacing (scaled -8)
                , verticalCenter
                , width fill
                ]
                [ -- 1.
                  --
                  if Maybe.isJust searchTerm then
                    el
                        (Tracks ClickableAction)
                        [ attribute "title" "Clear search"
                        , onClick (TracksMsg <| Tracks.Types.Search Nothing)
                        ]
                        (16
                            |> Material.Icons.Content.clear iconColor
                            |> html
                        )
                  else if Maybe.isJust maybeSelectedPlaylist then
                    el
                        (Tracks ClickableAction)
                        [ attribute "title" "Deactivate playlist"

                        --
                        , maybeSelectedPlaylist
                            |> Maybe.map (TogglePlaylist >> TracksMsg)
                            |> Maybe.withDefault NoOp
                            |> onClick
                        ]
                        (16
                            |> Material.Icons.Content.clear colors.base08
                            |> html
                        )
                  else
                    empty

                -- 2.
                --
                , el
                    (Tracks ClickableAction)
                    [ attribute "title" "Toggle favourites-only"
                    , onClick (TracksMsg ToggleFavouritesOnly)
                    , paddingRight (scaled 1)
                    ]
                    (case favouritesOnly of
                        True ->
                            html (Material.Icons.Action.favorite colors.base08 16)

                        False ->
                            html (Material.Icons.Action.favorite_border iconColor 16)
                    )
                ]
            ]
            (Input.text
                (Tracks Tracks.Styles.Search)
                [ inputBottomPadding
                , inputTopPadding
                , onBlur (TracksMsg <| Tracks.Types.Search searchTerm)
                , paddingLeft ((scaled -3) + 16 + (scaled -3))
                , width fill
                ]
                { onChange = TracksMsg << SetSearchTerm
                , value = Maybe.withDefault "" searchTerm
                , label =
                    Input.placeholder
                        { text = "Search"
                        , label = Input.hiddenLabel "search"
                        }
                , options = []
                }
            )

        -----------------------------------
        -- Part 2
        -----------------------------------
        , Navigation.insideCustom
            [ ( Icon Material.Icons.Editor.format_list_numbered
              , Label (Hidden "Playlists")
              , Playlists.Types.Index
                    |> Routing.Types.Playlists
                    |> Routing.Types.GoToPage
                    |> RoutingMsg
              )
            , ( Icon Material.Icons.Action.event_seat
              , Label (Hidden "Queue")
              , Queue.Types.Index
                    |> Routing.Types.Queue
                    |> Routing.Types.GoToPage
                    |> RoutingMsg
              )
            , ( Icon Material.Icons.Av.equalizer
              , Label (Hidden "Equalizer")
              , Routing.Types.Equalizer
                    |> Routing.Types.GoToPage
                    |> RoutingMsg
              )
            ]
        ]


content :
    List IdentifiedTrack
    -> ( SortBy, SortDirection, Maybe Playlist, Bool, IsProcessing, List Source )
    -> Node
content resultant ( sortBy, sortDirection, playlist, favouritesOnly, isProcessing, sources ) =
    el
        Zed
        [ attribute "data-favourites-only" (boolToAttr favouritesOnly)
        , clip
        , height fill
        ]
        (if List.isEmpty resultant then
            noTracksFound sources isProcessing
         else
            defaultTable resultant sortBy sortDirection
        )


scrollHandler : ScrollPos -> TopLevel.Msg
scrollHandler =
    ScrollThroughTableDebounced >> TopLevel.TracksMsg



-- Content messages


msgProcessing : Node
msgProcessing =
    el (Tracks Placeholder) [] (text "Processing Tracks")


msgNoSources : Node
msgNoSources =
    row
        Zed
        [ onWithOptions
            "click"
            { stopPropagation = False
            , preventDefault = True
            }
            (Sources.Types.New
                |> Routing.Types.Sources
                |> Routing.Types.GoToPage
                |> RoutingMsg
                |> Decode.succeed
            )
        , spacing (scaled -8)
        ]
        [ el
            WithoutLineHeight
            [ moveDown 1, verticalCenter ]
            (15
                |> Material.Icons.Content.add colors.base08
                |> html
            )
        , text "Add some music"
        ]
        |> el ImportantButton [ paddingXY (scaled -5) (scaled -6) ]
        |> link "/sources"


msgNoTracks : Node
msgNoTracks =
    el (Tracks Placeholder) [] (text "No tracks found")



-- Content views, Pt. 1


noTracksFound : List Source -> IsProcessing -> Node
noTracksFound sources isProcessing =
    within
        [ logoBackdrop
        , el
            Zed
            [ center, verticalCenter ]
            (case isProcessing of
                Just _ ->
                    msgProcessing

                Nothing ->
                    case List.length sources of
                        0 ->
                            msgNoSources

                        _ ->
                            msgNoTracks
            )
        ]
        (el
            Zed
            [ height fill, width fill ]
            empty
        )



-- Content views, Pt. 2


defaultTable : List IdentifiedTrack -> SortBy -> SortDirection -> Node
defaultTable tracks activeSortBy sortDirection =
    column
        (Tracks Table)
        [ clipX
        , height fill
        , id "tracks"
        , inlineStyle [ ( "border-radius", "3px" ) ]
        , onScroll scrollHandler
        , yScrollbar
        ]
        [ tracks
            |> List.map defaultTableItem
            |> Html.Keyed.ol [ Html.Attributes.class "tracks__table " ]
            |> html
        ]


defaultTableItem : IdentifiedTrack -> ( String, Html TopLevel.Msg )
defaultTableItem ( identifiers, track ) =
    ( track.id
    , Html.li
        [ Html.Attributes.rel
            (toString identifiers.indexInList)

        -----------------------------------
        -- Classes
        -----------------------------------
        , Html.Attributes.classList
            [ ( "tracks__tableRow", True )
            , ( "tracks__tableRow--alt", identifiers.indexInList % 2 == 1 )
            , ( "tracks__tableRow--isMissing", identifiers.isMissing )
            , ( "tracks__tableRow--isNotMissing", not identifiers.isMissing )
            , ( "tracks__tableRow--isNowPlaying", identifiers.isNowPlaying )
            ]
        ]
        [ -----------------------------------
          -- Favourite column
          -----------------------------------
          Html.div
            [ Html.Attributes.classList
                [ ( "tracks__tableFavouriteColumn", True )
                , ( "tracks__tableFavouriteColumn--isNowPlaying", identifiers.isNowPlaying )
                , ( "tracks__tableFavouriteColumn--isFavourite", identifiers.isFavourite )
                ]
            ]
            [ if identifiers.isFavourite then
                Html.text "t"
              else
                Html.text "f"
            ]

        -----------------------------------
        -- Other columns
        -----------------------------------
        , Html.div
            [ Html.Attributes.class "tracks__tableOtherColumns"
            , Html.Attributes.style [ ( "width", "37.5%" ) ]
            ]
            [ Html.text track.tags.title ]

        --
        , Html.div
            [ Html.Attributes.class "tracks__tableOtherColumns"
            , Html.Attributes.style [ ( "width", "29.0%" ) ]
            ]
            [ Html.text track.tags.artist ]

        --
        , Html.div
            [ Html.Attributes.class "tracks__tableOtherColumns"
            , Html.Attributes.style [ ( "width", "29.0%" ) ]
            ]
            [ Html.text track.tags.album ]
        ]
    )



-- Events {1}


playTrack : Decode.Decoder TopLevel.Msg
playTrack =
    Decode.map TopLevel.PlayTrack playTrackDecoder


selectTrack : Decode.Decoder TopLevel.Msg
selectTrack =
    Decode.map TopLevel.ApplyTrackSelection playTrackDecoder


playTrackDecoder : Decode.Decoder String
playTrackDecoder =
    presentTrackDecoder
        |> Decode.andThen (always trackRelDecoder)


toggleFavourite : Decode.Decoder TopLevel.Msg
toggleFavourite =
    Decode.map (ToggleFavourite >> TopLevel.TracksMsg) toggleFavouriteDecoder


toggleFavouriteDecoder : Decode.Decoder String
toggleFavouriteDecoder =
    Decode.string
        |> Decode.at [ "target", "attributes", "data-favourite", "value" ]
        |> Decode.andThen (always trackRelDecoder)


showContextMenu : Decode.Decoder TopLevel.Msg
showContextMenu =
    Decode.map TopLevel.ShowTrackContextMenu showContextMenuDecoder


showContextMenuDecoder : Decode.Decoder ( String, Mouse.Position )
showContextMenuDecoder =
    presentTrackDecoder
        |> Decode.andThen (always trackRelDecoder)
        |> Decode.andThen (\x -> Decode.andThen (Decode.succeed << (,) x) mousePositionDecoder)


showContextMenuOnTouch : Decode.Decoder TopLevel.Msg
showContextMenuOnTouch =
    Decode.map TopLevel.ShowTrackContextMenu showContextMenuOnTouchDecoder


showContextMenuOnTouchDecoder : Decode.Decoder ( String, Mouse.Position )
showContextMenuOnTouchDecoder =
    presentTrackDecoder
        |> Decode.andThen (always trackRelDecoder)
        |> Decode.andThen (\x -> Decode.andThen (Decode.succeed << (,) x) touchPositionDecoder)



-- Events {2}


presentTrackDecoder : Decode.Decoder String
presentTrackDecoder =
    Decode.andThen
        (\missingValue ->
            if missingValue == "f" then
                Decode.succeed "Track is present"
            else
                Decode.fail "Track is missing, invalid operation"
        )
        (Decode.oneOf
            [ Decode.at [ "target", "parentNode", "attributes", "data-missing", "value" ] Decode.string
            , Decode.at [ "target", "attributes", "data-missing", "value" ] Decode.string
            ]
        )


trackRelDecoder : Decode.Decoder String
trackRelDecoder =
    Decode.oneOf
        [ Decode.at
            [ "target", "parentNode", "attributes", "rel", "value" ]
            Decode.string
        , Decode.at
            [ "target", "attributes", "rel", "value" ]
            Decode.string
        ]


mousePositionDecoder : Decode.Decoder Mouse.Position
mousePositionDecoder =
    Decode.map2
        Mouse.Position
        (Decode.field "pageX" Decode.int)
        (Decode.field "pageY" Decode.int)


touchPositionDecoder : Decode.Decoder Mouse.Position
touchPositionDecoder =
    Decode.map2
        Mouse.Position
        (Decode.at [ "x" ] Decode.int)
        (Decode.at [ "y" ] Decode.int)


sortBy : SortBy -> TopLevel.Msg
sortBy =
    TopLevel.TracksMsg << SortBy



-- Scrolling


onScroll : (ScrollPos -> TopLevel.Msg) -> Attr
onScroll msg =
    on "scroll" (Decode.map msg decodeScrollPosition)


decodeScrollPosition : Decode.Decoder ScrollPos
decodeScrollPosition =
    Decode.map3
        ScrollPos
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "clientHeight" ] Decode.int)



-- Helpers


maybeShowSortIcon : SortBy -> SortBy -> Node -> Node
maybeShowSortIcon activeSortBy targetSortBy sortIcon =
    if targetSortBy == activeSortBy then
        sortIcon
    else
        empty


boolToAttr : Bool -> String
boolToAttr bool =
    case bool of
        True ->
            "t"

        False ->
            "f"
