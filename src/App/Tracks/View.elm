module Tracks.View exposing (entry)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onBlur, onClick, onInput, onSubmit)
import Html.Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode as Decode
import Material.Icons.Action
import Material.Icons.Av
import Material.Icons.Content
import Material.Icons.Navigation
import Material.Icons.Toggle
import Navigation.View as Navigation
import Sources.Types exposing (Source)
import Styles exposing (Classes(Button, ContentBox))
import Tracks.Styles exposing (..)
import Tracks.Types exposing (..)
import Types as TopLevel exposing (Model, Msg)
import Utils exposing (cssClass, cssClasses)
import Variables exposing (colors, colorDerivatives)


-- 🍯


entry : TopLevel.Model -> Html TopLevel.Msg
entry model =
    div
        [ entryClasses model.tracks.favouritesOnly ]
        [ lazy2
            navigation
            model.tracks.searchTerm
            model.tracks.favouritesOnly
        , lazy3
            content
            model.tracks.collectionExposed
            model.tracks.sortBy
            model.tracks.sortDirection
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
        [ Html.map
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
        , Navigation.insideCustom
            [ ( Material.Icons.Av.featured_play_list colorDerivatives.text 16, TopLevel.NoOp )
            ]
        ]


content : List IdentifiedTrack -> SortBy -> SortDirection -> Html TopLevel.Msg
content resultant sortBy sortDirection =
    div
        [ cssClass (TracksChild)
        , onScroll (ScrollThroughTable >> TopLevel.TracksMsg)
        ]
        [ if List.isEmpty resultant then
            div
                [ cssClass NoTracksFound ]
                [ text "No tracks found" ]
          else
            tracksTable resultant sortBy sortDirection
        ]



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
                (16)
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
                [ on "dblclick" playTrack, on "click" toggleFavourite ]
                (List.indexedMap tracksTableItem tracks)
            ]


tracksTableItem : Int -> IdentifiedTrack -> ( String, Html TopLevel.Msg )
tracksTableItem index ( identifiers, track ) =
    let
        key =
            toString index

        favAttr =
            case identifiers.isFavourite of
                True ->
                    "t"

                False ->
                    "f"
    in
        ( key
        , tr
            [ rel key ]
            [ td [ attribute "data-favourite" favAttr ] [ text "" ]
            , td [] [ text track.tags.title ]
            , td [] [ text track.tags.artist ]
            , td [] [ text track.tags.album ]
            ]
        )



-- Events and stuff


playTrack : Decode.Decoder TopLevel.Msg
playTrack =
    Decode.map TopLevel.PlayTrack tableTrackDecoder


tableTrackDecoder : Decode.Decoder String
tableTrackDecoder =
    Decode.oneOf
        [ Decode.at [ "target", "parentNode", "attributes", "rel", "value" ] Decode.string
        , Decode.at [ "target", "attributes", "rel", "value" ] Decode.string
        ]


toggleFavourite : Decode.Decoder TopLevel.Msg
toggleFavourite =
    Decode.map TopLevel.ToggleFavourite tableFavouriteDecoder


tableFavouriteDecoder : Decode.Decoder String
tableFavouriteDecoder =
    Decode.string
        |> Decode.at [ "target", "attributes", "data-favourite", "value" ]
        |> Decode.andThen
            (\_ ->
                Decode.at
                    [ "target", "parentNode", "attributes", "rel", "value" ]
                    Decode.string
            )


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
