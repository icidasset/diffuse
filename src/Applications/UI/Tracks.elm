module UI.Tracks exposing (initialModel, makeParcel, resolveParcel, update, view)

import Alien
import Chunky exposing (..)
import Color
import Color.Ext as Color
import Common exposing (Switch(..))
import Css
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes exposing (css, placeholder, tabindex, title, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Html.Styled.Ext exposing (onEnterKey)
import Html.Styled.Lazy exposing (..)
import InfiniteList
import Json.Decode as Json
import Json.Encode
import List.Extra as List
import Material.Icons.Action as Icons
import Material.Icons.Av as Icons
import Material.Icons.Content as Icons
import Material.Icons.Editor as Icons
import Material.Icons.Image as Icons
import Maybe.Extra as Maybe
import Replying as N5 exposing (R3D3)
import Return2 as R2
import Return3 as R3
import Tachyons.Classes as T
import Tracks exposing (..)
import Tracks.Collection as Collection exposing (..)
import Tracks.Encoding as Encoding
import Tracks.Favourites as Favourites
import UI.Core
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page exposing (Page)
import UI.Ports
import UI.Reply exposing (Reply(..))
import UI.Tracks.Core exposing (..)
import UI.Tracks.Scene.List



-- 🌳


initialModel : Model
initialModel =
    { collection = emptyCollection
    , enabledSourceIds = []
    , favourites = []
    , favouritesOnly = False
    , infiniteList = InfiniteList.init
    , nowPlaying = Nothing
    , scene = List
    , searchResults = Nothing
    , searchTerm = Nothing
    , sortBy = Artist
    , sortDirection = Asc
    }



-- 📣


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            R3.withNothing model

        InfiniteListMsg infiniteList ->
            R3.withNothing { model | infiniteList = infiniteList }

        Reply replies ->
            ( model, Cmd.none, Just replies )

        ScrollToNowPlaying ->
            case model.nowPlaying of
                Just identifiedTrack ->
                    case model.scene of
                        List ->
                            identifiedTrack
                                |> UI.Tracks.Scene.List.scrollToNowPlaying
                                |> R2.withModel model
                                |> R3.withNoReply

                Nothing ->
                    R3.withNothing model

        SetEnabledSourceIds sourceIds ->
            R3.withNothing { model | enabledSourceIds = sourceIds }

        SetNowPlaying maybeIdentifiedTrack ->
            let
                mapFn =
                    case maybeIdentifiedTrack of
                        Just a ->
                            \( i, t ) -> Tuple.pair { i | isNowPlaying = isNowPlaying a ( i, t ) } t

                        Nothing ->
                            \( i, t ) -> Tuple.pair { i | isNowPlaying = False } t
            in
            reviseCollection
                (map <| List.map mapFn)
                { model | nowPlaying = maybeIdentifiedTrack }

        SortBy property ->
            let
                sortDir =
                    if model.sortBy /= property then
                        Asc

                    else if model.sortDirection == Asc then
                        Desc

                    else
                        Asc
            in
            { model | sortBy = property, sortDirection = sortDir }
                |> reviseCollection arrange
                |> N5.addReply SaveEnclosedUserData

        -----------------------------------------
        -- Collection
        -----------------------------------------
        -- # Add
        -- > Add tracks to the collection.
        --
        Add json ->
            reviseCollection
                (json
                    |> Json.decodeValue (Json.list Encoding.trackDecoder)
                    |> Result.withDefault []
                    |> add
                )
                model

        -- # Remove
        -- > Remove tracks from the collection.
        --
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
            reviseCollection
                (removeByPaths sourceId paths)
                model

        RemoveBySourceId sourceId ->
            reviseCollection
                (removeBySourceId sourceId)
                model

        -----------------------------------------
        -- Favourites
        -----------------------------------------
        -- > Make a track a favourite, or remove it as a favourite
        ToggleFavourite index ->
            model.collection.harvested
                |> List.getAt index
                |> Maybe.map (toggleFavourite model)
                |> Maybe.withDefault (R3.withNothing model)

        -- > Filter collection by favourites only {toggle}
        ToggleFavouritesOnly ->
            { model | favouritesOnly = not model.favouritesOnly }
                |> reviseCollection harvest
                |> N5.addReply SaveEnclosedUserData

        -----------------------------------------
        -- Search
        -----------------------------------------
        ClearSearch ->
            { model | searchResults = Nothing, searchTerm = Nothing }
                |> reviseCollection harvest
                |> N5.addReply SaveEnclosedUserData

        Search ->
            case ( model.searchTerm, model.searchResults ) of
                ( Just term, _ ) ->
                    ( model
                    , UI.Ports.giveBrain Alien.SearchTracks (Json.Encode.string <| String.trim term)
                    , Nothing
                    )

                ( Nothing, Just _ ) ->
                    reviseCollection harvest { model | searchResults = Nothing }

                ( Nothing, Nothing ) ->
                    R3.withNothing model

        SetSearchResults json ->
            json
                |> Json.decodeValue (Json.list Json.string)
                |> Result.withDefault []
                |> (\results -> { model | searchResults = Just results })
                |> reviseCollection harvest
                |> N5.addReply (ToggleLoadingScreen Off)

        SetSearchTerm term ->
            R3.withReply
                [ SaveEnclosedUserData ]
                (case String.trim term of
                    "" ->
                        R2.withNoCmd { model | searchTerm = Nothing }

                    _ ->
                        R2.withNoCmd { model | searchTerm = Just term }
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

        oldHarvest =
            List.map (Tuple.second >> .id) model.collection.harvested

        newHarvest =
            List.map (Tuple.second >> .id) newCollection.harvested
    in
    ( modelWithNewCollection
      -- Command
      ----------
    , if oldHarvest /= newHarvest then
        case model.scene of
            List ->
                UI.Tracks.Scene.List.scrollToTop

      else
        Cmd.none
      -- Reply
      --------
    , (Just << Maybe.values)
        [ if model.collection.untouched /= newCollection.untouched then
            Just SaveTracks

          else
            Nothing

        --
        , if oldHarvest /= newHarvest then
            Just ResetQueue

          else
            Nothing
        ]
    )


reviseCollection : (Parcel -> Parcel) -> Model -> R3D3 Model Msg Reply
reviseCollection collector model =
    model
        |> makeParcel
        |> collector
        |> resolveParcel model



-- 📣  ░░  FAVOURITES


toggleFavourite : Model -> IdentifiedTrack -> R3D3 Model Msg Reply
toggleFavourite model ( i, t ) =
    let
        newFavourites =
            Favourites.toggleInFavouritesList ( i, t ) model.favourites

        effect =
            if model.favouritesOnly then
                Collection.map (Favourites.toggleInTracksList t) >> harvest

            else
                Collection.map (Favourites.toggleInTracksList t)
    in
    { model | favourites = newFavourites }
        |> reviseCollection effect
        |> N5.addReply SaveFavourites



-- 🗺


view : UI.Core.Model -> Html Msg
view core =
    chunk
        [ T.flex
        , T.flex_column
        , T.flex_grow_1
        ]
        [ lazy3
            navigation
            core.tracks.favouritesOnly
            core.tracks.searchTerm
            core.page

        --
        , if List.isEmpty core.tracks.collection.harvested then
            lazy4
                noTracksView
                core.sources.isProcessing
                (List.length core.sources.collection)
                (List.length core.tracks.collection.harvested)
                (List.length core.tracks.favourites)

          else
            case core.tracks.scene of
                List ->
                    UI.Tracks.Scene.List.view { height = core.viewport.height } core.tracks
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
    brick
        [ css navigationStyles ]
        [ T.flex, T.relative, T.z_4 ]
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


noTracksView : Bool -> Int -> Int -> Int -> Html Msg
noTracksView isProcessing amountOfSources amountOfTracks amountOfFavourites =
    UI.Kit.centeredContent
        [ if isProcessing then
            message "Processing Tracks"

          else if amountOfSources == 0 then
            chunk
                []
                [ UI.Kit.buttonLink
                    "/sources/new"
                    UI.Kit.Normal
                    (inline
                        [ UI.Kit.inlineIcon Icons.add
                        , text "Add some music"
                        ]
                    )
                , slab
                    Html.span
                    []
                    [ T.dib, T.w1 ]
                    []
                , UI.Kit.buttonWithColor
                    UI.Kit.colorKit.base0B
                    UI.Kit.Normal
                    (Reply [ InsertDemo ])
                    (inline
                        [ UI.Kit.inlineIcon Icons.music_note
                        , text "Insert demo"
                        ]
                    )
                ]

          else if amountOfTracks == 0 then
            message "No tracks found"

          else
            message "No sources available"
        ]


message : String -> Html Msg
message m =
    chunk
        [ T.bb, T.bw1, T.f6, T.fw6, T.lh_title, T.pb1 ]
        [ text m ]



-- 🖼


navigationStyles : List Css.Style
navigationStyles =
    [ Css.boxShadow5 (Css.px 0) (Css.px 0) (Css.px 10) (Css.px 1) (Css.rgba 0 0 0 0.05)
    ]


searchStyles : List Css.Style
searchStyles =
    [ Css.borderBottom3 (Css.px 1) Css.solid (Color.toElmCssColor UI.Kit.colors.subtleBorder)
    , Css.borderRight3 (Css.px 1) Css.solid (Color.toElmCssColor UI.Kit.colors.subtleBorder)
    ]


searchActionsStyles : List Css.Style
searchActionsStyles =
    [ Css.fontSize (Css.px 0)
    , Css.lineHeight (Css.px 0)
    , Css.marginTop (Css.px 1)
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
