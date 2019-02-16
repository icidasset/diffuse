module UI.Tracks exposing (Model, Msg(..), initialModel, makeParcel, resolveParcel, update, view)

import Alien
import Chunky exposing (..)
import Color
import Color.Ext as Color
import Css
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes exposing (css, placeholder, tabindex, title, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Html.Styled.Ext exposing (onEnterKey)
import Html.Styled.Lazy exposing (..)
import Json.Decode as Json
import Json.Encode
import Material.Icons.Action as Icons
import Material.Icons.Av as Icons
import Material.Icons.Content as Icons
import Material.Icons.Editor as Icons
import Replying exposing (R3D3)
import Return2
import Return3
import Tachyons.Classes as T
import Tracks exposing (..)
import Tracks.Collection exposing (..)
import Tracks.Encoding as Encoding
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page exposing (Page)
import UI.Ports
import UI.Reply exposing (Reply(..))



-- 🌳


type alias Model =
    { collection : Collection
    , enabledSourceIds : List String
    , favourites : List Favourite
    , favouritesOnly : Bool
    , nowPlaying : Maybe IdentifiedTrack
    , searchResults : Maybe (List String)
    , searchTerm : Maybe String
    , sortBy : SortBy
    , sortDirection : SortDirection
    }


initialModel : Model
initialModel =
    { collection = emptyCollection
    , enabledSourceIds = []
    , favourites = []
    , favouritesOnly = False
    , nowPlaying = Nothing
    , searchResults = Nothing
    , searchTerm = Nothing
    , sortBy = Artist
    , sortDirection = Asc
    }



-- 📣


type Msg
    = Bypass
    | SetEnabledSourceIds (List String)
      -----------------------------------------
      -- Collection
      -----------------------------------------
    | Add Json.Value
    | RemoveByPaths Json.Value
    | RemoveBySourceId String
      -----------------------------------------
      -- Favourites
      -----------------------------------------
    | ToggleFavouritesOnly
      -----------------------------------------
      -- Search
      -----------------------------------------
    | ClearSearch
    | Search
    | SetSearchResults Json.Value
    | SetSearchTerm String


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            Return3.withNothing model

        SetEnabledSourceIds sourceIds ->
            Return3.withNothing { model | enabledSourceIds = sourceIds }

        -----------------------------------------
        -- Collection
        -----------------------------------------
        -- # Add
        -- > Add tracks to the collection.
        --
        Add json ->
            let
                tracks =
                    json
                        |> Json.decodeValue (Json.list Encoding.trackDecoder)
                        |> Result.withDefault []
            in
            model
                |> makeParcel
                |> add tracks
                |> resolveParcel model

        -- # Remove
        -- > Remove tracks from the collection.
        RemoveByPaths json ->
            let
                decoder =
                    Json.map2
                        Tuple.pair
                        (Json.field "filePaths" <| Json.list Json.string)
                        (Json.field "sourceId" Json.string)

                ( paths, sourceId ) =
                    json
                        |> Json.decodeValue decoder
                        |> Result.withDefault ( [], missingId )
            in
            model
                |> makeParcel
                |> removeByPaths sourceId paths
                |> resolveParcel model

        RemoveBySourceId sourceId ->
            model
                |> makeParcel
                |> removeBySourceId sourceId
                |> resolveParcel model

        -----------------------------------------
        -- Favourites
        -----------------------------------------
        ToggleFavouritesOnly ->
            reviseCollection
                harvest
                { model | favouritesOnly = not model.favouritesOnly }

        -----------------------------------------
        -- Search
        -----------------------------------------
        ClearSearch ->
            reviseCollection
                harvest
                { model | searchResults = Nothing, searchTerm = Nothing }

        Search ->
            case ( model.searchTerm, model.searchResults ) of
                ( Just term, _ ) ->
                    ( model
                    , UI.Ports.giveBrain Alien.SearchTracks (Json.Encode.string term)
                    , Nothing
                    )

                ( Nothing, Just _ ) ->
                    reviseCollection harvest { model | searchResults = Nothing }

                ( Nothing, Nothing ) ->
                    Return3.withNothing model

        SetSearchResults json ->
            json
                |> Json.decodeValue (Json.list Json.string)
                |> Result.withDefault []
                |> (\results -> { model | searchResults = Just results })
                |> reviseCollection harvest

        SetSearchTerm term ->
            Return3.withNothing
                (case String.trim term of
                    "" ->
                        { model | searchTerm = Nothing }

                    t ->
                        { model | searchTerm = Just t }
                )



-- 📣  ░░  PARCEL


makeParcel : Model -> Parcel
makeParcel model =
    ( { enabledSourceIds = model.enabledSourceIds
      , favourites = model.favourites
      , favouritesOnly = model.favouritesOnly
      , nowPlaying = model.nowPlaying
      , searchResults = model.searchResults
      , sortBy = model.sortBy
      , sortDirection = model.sortDirection
      }
    , model.collection
    )


resolveParcel : Model -> Parcel -> R3D3 Model Msg Reply
resolveParcel model ( _, newCollection ) =
    let
        modelWithNewCollection =
            { model | collection = newCollection }
    in
    if model.collection.untouched /= newCollection.untouched then
        ( modelWithNewCollection
        , Cmd.none
        , Just [ SaveTracks ]
        )

    else
        Return3.withNothing modelWithNewCollection


reviseCollection : (Parcel -> Parcel) -> Model -> R3D3 Model Msg Reply
reviseCollection collector model =
    model
        |> makeParcel
        |> collector
        |> resolveParcel model



-- 🗺


view : Page -> Model -> Html Msg
view page model =
    chunk
        []
        [ lazy3
            navigation
            model.favouritesOnly
            model.searchTerm
            page

        --
        , chunk
            []
            (List.map
                (\( _, t ) -> text <| t.tags.artist ++ " - " ++ t.tags.title)
                model.collection.harvested
            )
        ]


navigation : Bool -> Maybe String -> Page -> Html Msg
navigation favouritesOnly searchTerm page =
    let
        tabindex_ =
            case page of
                UI.Page.Index ->
                    0

                _ ->
                    -1
    in
    chunk
        [ T.flex ]
        [ -----------------------------------------
          -- Part 1
          -----------------------------------------
          brick
            [ css searchStyles ]
            [ T.flex_grow_1
            , T.overflow_hidden
            , T.relative
            ]
            [ -- Input
              --------
              slab
                Html.input
                [ css searchInputStyles
                , onBlur Search
                , onEnterKey Search
                , onInput SetSearchTerm
                , placeholder "Search"
                , tabindex tabindex_
                , value (Maybe.withDefault "" searchTerm)
                ]
                [ T.bg_transparent
                , T.bn
                , T.flex
                , T.h_100
                , T.items_center
                , T.outline_0
                , T.relative
                , T.w_100
                , T.z_1
                ]
                []

            -- Search icon
            --------------
            , brick
                [ css searchIconStyles ]
                [ T.absolute
                , T.bottom_0
                , T.flex
                , T.items_center
                , T.left_0
                , T.top_0
                , T.z_0
                ]
                [ Html.fromUnstyled (Icons.search searchIconColor 16) ]

            -- Actions
            ----------
            , brick
                [ css searchActionsStyles ]
                [ T.absolute
                , T.flex
                , T.items_center
                , T.right_0
                , T.z_2
                ]
                [ -- 1
                  case searchTerm of
                    Just _ ->
                        brick
                            [ css searchActionIconStyle
                            , onClick ClearSearch
                            , title "Clear search"
                            ]
                            [ T.pointer ]
                            [ Html.fromUnstyled (Icons.clear searchIconColor 16) ]

                    Nothing ->
                        nothing

                -- 2
                , brick
                    [ css searchActionIconStyle
                    , onClick ToggleFavouritesOnly
                    , title "Toggle favourites-only"
                    ]
                    [ T.pointer ]
                    [ case favouritesOnly of
                        True ->
                            Html.fromUnstyled (Icons.favorite UI.Kit.colorKit.base08 16)

                        False ->
                            Html.fromUnstyled (Icons.favorite_border searchIconColor 16)
                    ]

                -- 3
                , nothing
                ]
            ]
        , -----------------------------------------
          -- Part 2
          -----------------------------------------
          UI.Navigation.localWithTabindex
            tabindex_
            [ ( Icon Icons.format_list_numbered
              , Label "Playlists" Hidden
              , PerformMsg Bypass
              )
            , ( Icon Icons.event_seat
              , Label "Queue" Hidden
              , PerformMsg Bypass
              )
            , ( Icon Icons.equalizer
              , Label "Equalizer" Hidden
              , PerformMsg Bypass
              )
            ]
        ]



-- 🖼


searchStyles : List Css.Style
searchStyles =
    [ Css.borderBottom3 (Css.px 1) Css.solid (Color.toElmCssColor UI.Kit.colors.subtleBorder)
    , Css.borderRight3 (Css.px 1) Css.solid (Color.toElmCssColor UI.Kit.colors.subtleBorder)
    ]


searchActionsStyles : List Css.Style
searchActionsStyles =
    [ Css.marginTop (Css.px 1)
    , Css.paddingRight (Css.px <| 13 - 6)
    , Css.top (Css.pct 50)
    , Css.transform (Css.translateY <| Css.pct -50)
    ]


searchActionIconStyle : List Css.Style
searchActionIconStyle =
    [ Css.marginRight (Css.px 6) ]


searchIconColor : Color.Color
searchIconColor =
    Color.rgb255 205 205 205


searchIconStyles : List Css.Style
searchIconStyles =
    [ Css.paddingLeft (Css.px 13) ]


searchInputStyles : List Css.Style
searchInputStyles =
    [ Css.paddingLeft (Css.px <| 13 + 16 + 9)
    , Css.fontSize (Css.px 14)
    , Css.height (Css.pct 98)
    ]
