module Tracks.View exposing (entry)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onBlur, onClick, onInput, onSubmit, onWithOptions)
import Html.Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Decode
import Material.Icons.Action
import Material.Icons.Av
import Material.Icons.Content
import Material.Icons.Navigation
import Material.Icons.Toggle
import Mouse
import Navigation.View as Navigation
import Routing.Types exposing (Msg(..))
import Sources.Types exposing (IsProcessing, Source)
import Styles exposing (Classes(Button, ContentBox, Important, LogoBackdrop))
import Tracks.Styles exposing (..)
import Tracks.Types exposing (..)
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (cssClass, cssClasses)
import Variables exposing (colorDerivatives, colors)


-- 🍯


entry : TopLevel.Model -> Html TopLevel.Msg
entry model =
    div
        [ entryClasses model.tracks.favouritesOnly ]
        [ lazy2
            navigation
            model.tracks.searchTerm
            model.tracks.favouritesOnly
        , lazy2
            content
            model.tracks.collection.exposed
            ( model.tracks.sortBy
            , model.tracks.sortDirection
            , model.sources.isProcessing
            , model.sources.collection
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


navigation : Maybe String -> Bool -> Html TopLevel.Msg
navigation searchTerm favouritesOnly =
    div
        [ cssClass TracksNavigation ]
        [ --
          -- Part 1
          --
          Html.map
            TopLevel.TracksMsg
            (Html.form
                [ onSubmit (Search searchTerm) ]
                [ -- Input
                  input
                    [ onBlur (Search searchTerm)
                    , onInput SetSearchTerm
                    , placeholder "Search"
                    , value (Maybe.withDefault "" searchTerm)
                    ]
                    []

                -- Search icon
                , span
                    [ cssClass TracksNavigationIcon ]
                    [ Material.Icons.Action.search
                        (Color.rgb 205 205 205)
                        16
                    ]

                -- Clear icon
                , span
                    [ cssClass TracksNavigationIcon
                    , onClick (Search Nothing)
                    , title "Clear search"
                    ]
                    (case searchTerm of
                        Just _ ->
                            [ Material.Icons.Content.clear
                                (Color.rgb 205 205 205)
                                16
                            ]

                        Nothing ->
                            []
                    )

                -- Favourites-only icon
                , span
                    [ cssClass TracksNavigationIcon
                    , onClick ToggleFavouritesOnly
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
                ]
            )

        --
        -- Part 2
        --
        , Navigation.insideCustom
            [ ( Material.Icons.Av.equalizer colorDerivatives.text 16
              , TopLevel.RoutingMsg (GoToUrl "/equalizer")
              )
            , ( Material.Icons.Av.featured_play_list colorDerivatives.text 16
              , TopLevel.ShowViewMenu
              )
            ]
        ]


content :
    List IdentifiedTrack
    -> ( SortBy, SortDirection, IsProcessing, List Source )
    -> Html TopLevel.Msg
content resultant ( sortBy, sortDirection, isProcessing, sources ) =
    div
        [ cssClass TracksChild
        , onScroll (ScrollThroughTable >> TopLevel.TracksMsg)
        , id "tracks"
        ]
        [ if List.isEmpty resultant then
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
          else
            tracksTable resultant sortBy sortDirection
        ]



-- Content messages


msgProcessing : Html TopLevel.Msg
msgProcessing =
    text "Processing Tracks ..."


msgNoSources : Html TopLevel.Msg
msgNoSources =
    a
        [ cssClass Important
        , href "/sources/new"
        , onWithOptions
            "click"
            { stopPropagation = False
            , preventDefault = True
            }
            ("/sources/new"
                |> GoToUrl
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



-- Content views


tracksTable : List IdentifiedTrack -> SortBy -> SortDirection -> Html TopLevel.Msg
tracksTable tracks activeSortBy sortDirection =
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
                [ on "dblclick" playTrack
                , on "click" toggleFavourite
                , onWithOptions
                    "contextmenu"
                    { stopPropagation = True
                    , preventDefault = True
                    }
                    showContextMenu
                ]
                (List.indexedMap tracksTableItem tracks)
            ]


tracksTableItem : Int -> IdentifiedTrack -> ( String, Html TopLevel.Msg )
tracksTableItem index ( identifiers, track ) =
    let
        key =
            toString index
    in
        ( key
        , tr
            [ rel key
            , attribute "data-missing" (boolToAttr identifiers.isMissing)
            , attribute "data-nowplaying" (boolToAttr identifiers.isNowPlaying)
            ]
            [ td [ attribute "data-favourite" (boolToAttr identifiers.isFavourite) ] []
            , td [] [ text track.tags.title ]
            , td [] [ text track.tags.artist ]
            , td [] [ text track.tags.album ]
            ]
        )



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
    Decode.map TopLevel.ToggleFavourite toggleFavouriteDecoder


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
