module Tracks.View exposing (entry)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onBlur, onClick, onInput, onSubmit, onWithOptions)
import Html.Keyed
import Html.Lazy exposing (lazy2, lazy3)
import Json.Decode as Decode
import List.Extra as List
import Material.Icons.Action
import Material.Icons.Av
import Material.Icons.Content
import Material.Icons.Editor
import Material.Icons.Image
import Material.Icons.Navigation
import Maybe.Extra as Maybe
import Mouse
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Playlists.Types exposing (Playlist)
import Playlists.Utils exposing (..)
import Queue.Types
import Routing.Types
import Sources.Types exposing (IsProcessing, Source)
import Styles exposing (Classes(Important, LogoBackdrop))
import Tracks.Styles exposing (..)
import Tracks.Types exposing (..)
import Types as TopLevel exposing (Msg(..))
import Utils exposing (cssClass, cssClasses)
import Variables exposing (colors)


-- 🍯


entry : TopLevel.Model -> Html TopLevel.Msg
entry model =
    div
        [ entryClasses model.tracks.favouritesOnly ]
        [ lazy3
            navigation
            model.tracks.searchTerm
            model.tracks.favouritesOnly
            model.tracks.selectedPlaylist
        , lazy2
            content
            model.tracks.collection.exposed
            ( model.tracks.sortBy
            , model.tracks.sortDirection
            , model.tracks.selectedPlaylist
            , model.sources.isProcessing
            , model.sources.collection
            , model.isTouchDevice
            )
        ]


entryClasses : Bool -> Attribute TopLevel.Msg
entryClasses favouritesOnly =
    case favouritesOnly of
        True ->
            cssClasses [ TracksContainer, FavouritesOnly ]

        False ->
            cssClasses [ TracksContainer ]



-- Views


navigation : Maybe String -> Bool -> Maybe Playlist -> Html TopLevel.Msg
navigation searchTerm favouritesOnly maybeSelectedPlaylist =
    div
        [ cssClass TracksNavigation ]
        [ --
          -- Part 1
          --
          Html.form
            [ onSubmit (TracksMsg <| Search searchTerm) ]
            [ -- Input
              input
                [ onBlur (TracksMsg <| Search searchTerm)
                , onInput (TracksMsg << SetSearchTerm)
                , placeholder "Search"
                , value (Maybe.withDefault "" searchTerm)
                ]
                []

            -- Search icon
            , a
                [ cssClass TracksNavigationIcon
                , style [ ( "cursor", "default" ) ]
                ]
                [ Material.Icons.Action.search
                    (Color.rgb 205 205 205)
                    16
                ]

            -- Favourites-only icon
            , a
                [ cssClass TracksNavigationIcon
                , onClick (TracksMsg <| ToggleFavouritesOnly)
                , title "Toggle favourites-only"
                ]
                (case favouritesOnly of
                    True ->
                        [ Material.Icons.Action.favorite
                            colors.base08
                            16
                        ]

                    False ->
                        [ Material.Icons.Action.favorite_border
                            (Color.rgb 205 205 205)
                            16
                        ]
                )

            -- Clear icon
            , if Maybe.isJust searchTerm then
                a
                    [ cssClass TracksNavigationIcon
                    , title "Clear search"
                    , onClick (TracksMsg <| Search Nothing)
                    ]
                    [ Material.Icons.Content.clear
                        (Color.rgb 205 205 205)
                        16
                    ]
              else if Maybe.isJust maybeSelectedPlaylist then
                a
                    [ cssClass TracksNavigationIcon
                    , title "Deactivate playlist"

                    --
                    , maybeSelectedPlaylist
                        |> Maybe.map (TogglePlaylist >> TracksMsg)
                        |> Maybe.withDefault NoOp
                        |> onClick
                    ]
                    [ Material.Icons.Content.clear
                        colors.base08
                        16
                    ]
              else
                text ""
            ]

        --
        -- Part 2
        --
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
    -> ( SortBy, SortDirection, Maybe Playlist, IsProcessing, List Source, Bool )
    -> Html TopLevel.Msg
content resultant ( sortBy, sortDirection, playlist, isProcessing, sources, isTouchDevice ) =
    div
        [ cssClass TracksChild
        , onScroll (ScrollThroughTable >> TopLevel.TracksMsg)
        , id "tracks"
        ]
        [ if List.isEmpty resultant then
            noTracksFound sources isProcessing
          else
            case Maybe.map .autoGenerated playlist of
                Just False ->
                    tracksTablePlaylist resultant isTouchDevice

                _ ->
                    tracksTableDefault resultant sortBy sortDirection isTouchDevice
        ]



-- Content messages


msgProcessing : Html TopLevel.Msg
msgProcessing =
    text "Processing Tracks"


msgNoSources : Html TopLevel.Msg
msgNoSources =
    a
        [ cssClass Important
        , href "/sources"
        , onWithOptions
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
        ]
        [ span
            [ style
                [ ( "margin-right", "0.375rem" )
                , ( "position", "relative" )
                , ( "top", "2px" )
                ]
            ]
            [ Material.Icons.Content.add
                (Color.rgb 185 182 176)
                15
            ]
        , text "Add some music"
        ]


msgNoTracks : Html TopLevel.Msg
msgNoTracks =
    text "No tracks found"



-- Content views, Pt. 1


noTracksFound : List Source -> IsProcessing -> Html TopLevel.Msg
noTracksFound sources isProcessing =
    div
        []
        [ div
            [ cssClasses
                (case List.length sources of
                    0 ->
                        [ NoTracksFound ]

                    _ ->
                        [ NoTracksFound, NoTracksFoundUnderline ]
                )
            ]
            [ case isProcessing of
                Just _ ->
                    msgProcessing

                Nothing ->
                    case List.length sources of
                        0 ->
                            msgNoSources

                        _ ->
                            msgNoTracks
            ]
        , div
            [ cssClass LogoBackdrop ]
            []
        ]



-- Content views, Pt. 2


tracksTableDefault : List IdentifiedTrack -> SortBy -> SortDirection -> Bool -> Html TopLevel.Msg
tracksTableDefault tracks activeSortBy sortDirection isTouchDevice =
    let
        sortIcon =
            (if sortDirection == Desc then
                Material.Icons.Navigation.expand_less
             else
                Material.Icons.Navigation.expand_more
            )
                (Color.rgb 207 207 207)
                16
    in
        table
            [ cssClass TracksTable ]
            [ thead
                []
                [ th
                    [ style [ ( "width", "4.50%" ) ] ]
                    []
                , th
                    [ style [ ( "width", "37.5%" ) ], onClick (sortBy Title) ]
                    [ text "Title", maybeShowSortIcon activeSortBy Title sortIcon ]
                , th
                    [ style [ ( "width", "29.0%" ) ], onClick (sortBy Artist) ]
                    [ text "Artist", maybeShowSortIcon activeSortBy Artist sortIcon ]
                , th
                    [ style [ ( "width", "29.0%" ) ], onClick (sortBy Album) ]
                    [ text "Album", maybeShowSortIcon activeSortBy Album sortIcon ]
                ]
            , Html.Keyed.node
                "tbody"
                (tracksTableWrapperAttr isTouchDevice)
                (List.indexedMap tracksTableItemDefault tracks)
            ]


tracksTablePlaylist : List IdentifiedTrack -> Bool -> Html TopLevel.Msg
tracksTablePlaylist tracks isTouchDevice =
    table
        [ cssClass TracksTable ]
        [ thead
            []
            [ th
                [ style [ ( "width", "4.50%" ) ] ]
                []
            , th
                [ style [ ( "width", "6.00%" ) ] ]
                [ text "#" ]
            , th
                [ style [ ( "width", "36.5%" ) ] ]
                [ text "Title" ]
            , th
                [ style [ ( "width", "28.0%" ) ] ]
                [ text "Artist" ]
            , th
                [ style [ ( "width", "28.0%" ) ] ]
                [ text "Album" ]
            ]
        , Html.Keyed.node
            "tbody"
            (tracksTableWrapperAttr isTouchDevice)
            (List.indexedMap tracksTableItemPlaylist tracks)
        ]


tracksTableItemDefault : Int -> IdentifiedTrack -> ( String, Html TopLevel.Msg )
tracksTableItemDefault index ( identifiers, track ) =
    ( track.id
    , tr
        [ rel (toString index)
        , attribute "data-missing" (boolToAttr identifiers.isMissing)
        , attribute "data-nowplaying" (boolToAttr identifiers.isNowPlaying)
        ]
        [ td [ attribute "data-favourite" (boolToAttr identifiers.isFavourite) ] []
        , td [] [ text track.tags.title ]
        , td [] [ text track.tags.artist ]
        , td [] [ text track.tags.album ]
        ]
    )


tracksTableItemPlaylist : Int -> IdentifiedTrack -> ( String, Html TopLevel.Msg )
tracksTableItemPlaylist index ( identifiers, track ) =
    ( track.id
    , tr
        [ rel (toString index)
        , attribute "data-missing" (boolToAttr identifiers.isMissing)
        , attribute "data-nowplaying" (boolToAttr identifiers.isNowPlaying)
        ]
        [ td [ attribute "data-favourite" (boolToAttr identifiers.isFavourite) ] []
        , td []
            [ identifiers.indexInPlaylist
                |> Maybe.withDefault 0
                |> (+) 1
                |> toString
                |> text
            ]
        , td [] [ text track.tags.title ]
        , td [] [ text track.tags.artist ]
        , td [] [ text track.tags.album ]
        ]
    )


tracksTableWrapperAttr : Bool -> List (Attribute TopLevel.Msg)
tracksTableWrapperAttr isTouchDevice =
    if isTouchDevice then
        -- Touch devices
        [ on "dbltap" playTrack
        , on "tap" toggleFavourite
        , onWithOptions
            "longtap"
            { stopPropagation = True
            , preventDefault = True
            }
            showContextMenuOnTouch
        , onWithOptions
            "touchend"
            { stopPropagation = False
            , preventDefault = True
            }
            (Decode.succeed TopLevel.NoOp)
        ]
    else
        -- Non-Touch devices
        [ on "dblclick" playTrack
        , on "click" toggleFavourite
        , onWithOptions
            "contextmenu"
            { stopPropagation = True
            , preventDefault = True
            }
            showContextMenu
        ]



-- Events {1}


playTrack : Decode.Decoder TopLevel.Msg
playTrack =
    Decode.map TopLevel.PlayTrack playTrackDecoder


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


onScroll : (ScrollPos -> msg) -> Attribute msg
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


maybeShowSortIcon : SortBy -> SortBy -> Html TopLevel.Msg -> Html TopLevel.Msg
maybeShowSortIcon activeSortBy targetSortBy sortIcon =
    if targetSortBy == activeSortBy then
        sortIcon
    else
        text ""


boolToAttr : Bool -> String
boolToAttr bool =
    case bool of
        True ->
            "t"

        False ->
            "f"
