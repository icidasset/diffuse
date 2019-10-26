module UI.Tracks exposing (Dependencies, Model, Msg(..), Scene(..), importHypaethral, initialModel, update, view)

import Alien
import Chunky exposing (..)
import Classes as C
import Color exposing (Color)
import Color.Ext as Color
import Common exposing (Switch(..))
import Conditional exposing (ifThenElse)
import Coordinates exposing (Coordinates, Viewport)
import Css
import Css.Transitions exposing (transition)
import Html.Events.Extra.Mouse as Mouse
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes exposing (css, fromUnstyled, href, placeholder, tabindex, target, title, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Html.Styled.Ext exposing (onEnterKey)
import Html.Styled.Lazy exposing (..)
import InfiniteList
import Json.Decode as Json
import Json.Encode
import List.Ext as List
import List.Extra as List
import Material.Icons exposing (Coloring(..))
import Material.Icons.Action as Icons
import Material.Icons.Av as Icons
import Material.Icons.Communication as Icons
import Material.Icons.Content as Icons
import Material.Icons.Editor as Icons
import Material.Icons.Image as Icons
import Material.Icons.Navigation as Icons
import Maybe.Extra as Maybe
import Playlists exposing (Playlist)
import Return3 as Return exposing (..)
import Sources exposing (Source)
import Tachyons.Classes as T
import Task.Extra as Task
import Tracks exposing (..)
import Tracks.Collection as Collection exposing (..)
import Tracks.Encoding as Encoding
import Tracks.Favourites as Favourites
import UI.DnD as DnD
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page exposing (Page)
import UI.Playlists.Page
import UI.Ports
import UI.Queue.Page
import UI.Reply as UI exposing (Reply(..))
import UI.Sources.Page as Sources
import UI.Tracks.Reply as Tracks exposing (Reply(..))
import UI.Tracks.Scene.List
import User.Layer exposing (HypaethralData)



-- 🌳


type alias Model =
    { cached : List String
    , cachedOnly : Bool
    , cachingInProgress : List String
    , collection : Collection
    , enabledSourceIds : List String
    , favourites : List Favourite
    , favouritesOnly : Bool
    , grouping : Maybe Grouping
    , hideDuplicates : Bool
    , nowPlaying : Maybe IdentifiedTrack
    , scene : Scene
    , searchResults : Maybe (List String)
    , searchTerm : Maybe String
    , selectedPlaylist : Maybe Playlist
    , selectedTrackIndexes : List Int
    , sortBy : SortBy
    , sortDirection : SortDirection

    -----------------------------------------
    -- Scenes
    -----------------------------------------
    , listScene : UI.Tracks.Scene.List.Model
    }


type Scene
    = List


initialModel : Model
initialModel =
    { cached = []
    , cachedOnly = False
    , cachingInProgress = []
    , collection = emptyCollection
    , enabledSourceIds = []
    , favourites = []
    , favouritesOnly = False
    , grouping = Nothing
    , hideDuplicates = False
    , nowPlaying = Nothing
    , scene = List
    , searchResults = Nothing
    , searchTerm = Nothing
    , selectedPlaylist = Nothing
    , selectedTrackIndexes = []
    , sortBy = Artist
    , sortDirection = Asc

    -----------------------------------------
    -- Scenes
    -----------------------------------------
    , listScene = UI.Tracks.Scene.List.initialModel
    }



-- 📣


type Msg
    = Bypass
    | Harvest
    | Reply UI.Reply
    | ScrollToNowPlaying
    | SetEnabledSourceIds (List String)
    | SetNowPlaying (Maybe IdentifiedTrack)
    | ToggleCachedOnly
    | ToggleFavouritesOnly
    | ToggleHideDuplicates
      -----------------------------------------
      -- Collection
      -----------------------------------------
    | Add Json.Value
    | RemoveByPaths Json.Value
    | RemoveBySourceId String
      -----------------------------------------
      -- Groups
      -----------------------------------------
    | DisableGrouping
    | GroupBy Grouping
      -----------------------------------------
      -- Menus
      -----------------------------------------
    | ShowTrackMenu Int { alt : Bool } Coordinates
    | ShowViewMenu (Maybe Grouping) Mouse.Event
      -----------------------------------------
      -- Playlists
      -----------------------------------------
    | DeselectPlaylist
    | SelectPlaylist Playlist
      -----------------------------------------
      -- Scenes
      -----------------------------------------
    | ListSceneMsg UI.Tracks.Scene.List.Msg
      -----------------------------------------
      -- Search
      -----------------------------------------
    | ClearSearch
    | Search
    | SetSearchResults Json.Value
    | SetSearchTerm String


update : Msg -> Model -> Return Model Msg UI.Reply
update msg model =
    case msg of
        Bypass ->
            return model

        Harvest ->
            reviseCollection harvest model

        Reply reply ->
            returnReplyWithModel model reply

        ScrollToNowPlaying ->
            let
                -- The index identifier might be out-of-date,
                -- so we get the latest version.
                it =
                    model.nowPlaying
                        |> Maybe.map (Tuple.second >> .id)
                        |> Maybe.andThen
                            (\id ->
                                List.find
                                    (Tuple.second >> .id >> (==) id)
                                    model.collection.harvested
                            )
            in
            model.nowPlaying
                |> Maybe.map (Tuple.second >> .id)
                |> Maybe.andThen
                    (\id ->
                        List.find
                            (Tuple.second >> .id >> (==) id)
                            model.collection.harvested
                    )
                |> Maybe.map
                    (case model.scene of
                        List ->
                            UI.Tracks.Scene.List.scrollToNowPlaying model.collection.harvested
                                >> Cmd.map ListSceneMsg
                    )
                |> Maybe.map
                    (\cmd ->
                        cmd
                            |> Return.commandWithModel model
                            |> Return.addReply (GoToPage UI.Page.Index)
                    )
                |> Maybe.withDefault (return model)

        SetEnabledSourceIds sourceIds ->
            reviseCollection identify
                { model | enabledSourceIds = sourceIds }

        SetNowPlaying maybeIdentifiedTrack ->
            return { model | nowPlaying = maybeIdentifiedTrack }

        ToggleCachedOnly ->
            { model | cachedOnly = not model.cachedOnly }
                |> reviseCollection harvest
                |> addReply SaveEnclosedUserData

        ToggleFavouritesOnly ->
            { model | favouritesOnly = not model.favouritesOnly }
                |> reviseCollection harvest
                |> addReply SaveEnclosedUserData

        ToggleHideDuplicates ->
            { model | hideDuplicates = not model.hideDuplicates }
                |> reviseCollection arrange
                |> addReply SaveSettings

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

                { kept, removed } =
                    Tracks.removeByPaths
                        { sourceId = sourceId, paths = paths }
                        model.collection.untouched

                newCollection =
                    { emptyCollection | untouched = kept }
            in
            { model | collection = newCollection }
                |> reviseCollection identify
                |> addReply (RemoveTracksFromCache removed)

        RemoveBySourceId sourceId ->
            let
                { kept, removed } =
                    Tracks.removeBySourceId sourceId model.collection.untouched

                newCollection =
                    { emptyCollection | untouched = kept }
            in
            { model | collection = newCollection }
                |> reviseCollection identify
                |> addReply (RemoveTracksFromCache removed)

        -----------------------------------------
        -- Groups
        -----------------------------------------
        DisableGrouping ->
            { model | grouping = Nothing }
                |> reviseCollection arrange
                |> addReply SaveEnclosedUserData

        GroupBy grouping ->
            { model | grouping = Just grouping }
                |> reviseCollection arrange
                |> addReply SaveEnclosedUserData

        -----------------------------------------
        -- Menus
        -----------------------------------------
        ShowTrackMenu trackIndex options coordinates ->
            let
                listScene =
                    model.listScene

                selection =
                    if List.isEmpty model.selectedTrackIndexes then
                        [ trackIndex ]

                    else if List.member trackIndex model.selectedTrackIndexes == False then
                        [ trackIndex ]

                    else
                        model.selectedTrackIndexes
            in
            model.collection.harvested
                |> List.pickIndexes selection
                |> ShowTracksContextMenu coordinates options
                |> returnReplyWithModel
                    { model
                        | listScene = { listScene | dnd = DnD.initialModel }
                        , selectedTrackIndexes = selection
                    }

        ShowViewMenu grouping mouseEvent ->
            grouping
                |> ShowTracksViewMenu (Coordinates.fromTuple mouseEvent.clientPos)
                |> returnReplyWithModel model

        -----------------------------------------
        -- Playlists
        -----------------------------------------
        DeselectPlaylist ->
            { model | selectedPlaylist = Nothing }
                |> reviseCollection arrange
                |> addReply SaveEnclosedUserData

        SelectPlaylist playlist ->
            { model | selectedPlaylist = Just playlist }
                |> reviseCollection arrange
                |> addReply SaveEnclosedUserData

        -----------------------------------------
        -- Scenes
        -----------------------------------------
        ListSceneMsg sub ->
            Return.castNested
                translateReply
                { mapCmd = ListSceneMsg
                , mapModel = \child -> { model | listScene = child }
                , update = UI.Tracks.Scene.List.update
                }
                { model = model.listScene
                , msg = sub
                }

        -----------------------------------------
        -- Search
        -----------------------------------------
        ClearSearch ->
            { model | searchResults = Nothing, searchTerm = Nothing }
                |> reviseCollection harvest
                |> addReply SaveEnclosedUserData

        Search ->
            case ( model.searchTerm, model.searchResults ) of
                ( Just term, _ ) ->
                    term
                        |> String.trim
                        |> Json.Encode.string
                        |> UI.Ports.giveBrain Alien.SearchTracks
                        |> Return.commandWithModel model

                ( Nothing, Just _ ) ->
                    reviseCollection harvest { model | searchResults = Nothing }

                ( Nothing, Nothing ) ->
                    return model

        SetSearchResults json ->
            case model.searchTerm of
                Just _ ->
                    json
                        |> Json.decodeValue (Json.list Json.string)
                        |> Result.withDefault []
                        |> (\results -> { model | searchResults = Just results })
                        |> reviseCollection harvest
                        |> addReply (ToggleLoadingScreen Off)

                Nothing ->
                    return model

        SetSearchTerm term ->
            addReplies
                [ SaveEnclosedUserData ]
                (case String.trim term of
                    "" ->
                        return { model | searchTerm = Nothing }

                    _ ->
                        return { model | searchTerm = Just term }
                )


importHypaethral : Model -> HypaethralData -> Maybe Playlist -> Return Model Msg UI.Reply
importHypaethral model data selectedPlaylist =
    let
        adjustedModel =
            { model
                | collection = { emptyCollection | untouched = data.tracks }
                , enabledSourceIds = Sources.enabledSourceIds data.sources
                , favourites = data.favourites
                , hideDuplicates = Maybe.unwrap False .hideDuplicates data.settings
                , selectedPlaylist = selectedPlaylist
            }

        addReplyIfNecessary =
            case model.searchTerm of
                Just _ ->
                    identity

                Nothing ->
                    addReply (ToggleLoadingScreen Off)
    in
    adjustedModel
        |> makeParcel
        |> identify
        |> resolveParcel adjustedModel
        |> andThen (update Search)
        |> addReplyIfNecessary



-- 📣  ░░  CHILDREN & REPLIES


translateReply : Tracks.Reply -> Model -> Return Model Msg UI.Reply
translateReply reply model =
    case reply of
        Transcend uiReplies ->
            returnRepliesWithModel model uiReplies

        --
        MarkAsSelected indexInList { shiftKey } ->
            let
                selection =
                    if shiftKey then
                        model.selectedTrackIndexes
                            |> List.head
                            |> Maybe.map
                                (\n ->
                                    if n > indexInList then
                                        List.range indexInList n

                                    else
                                        List.range n indexInList
                                )
                            |> Maybe.withDefault [ indexInList ]

                    else
                        [ indexInList ]
            in
            return { model | selectedTrackIndexes = selection }

        MoveTrackInSelectedPlaylist moveFromTo ->
            case model.selectedPlaylist of
                Just p ->
                    let
                        updatedPlaylist =
                            { p | tracks = List.move moveFromTo p.tracks }
                    in
                    { model | selectedPlaylist = Just updatedPlaylist }
                        |> reviseCollection arrange
                        |> addReply (ReplacePlaylistInCollection updatedPlaylist)

                Nothing ->
                    return model

        ShowTrackMenuWithoutDelay a b c ->
            update (ShowTrackMenu a b c) model

        ShowTrackMenuWithSmallDelay a b c ->
            ShowTrackMenu a b c
                |> Task.doDelayed 250
                |> returnCommandWithModel model

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
                |> addReply SaveEnclosedUserData

        ToggleFavourite index ->
            model.collection.harvested
                |> List.getAt index
                |> Maybe.map (toggleFavourite model)
                |> Maybe.withDefault (return model)



-- 📣  ░░  PARCEL


makeParcel : Model -> Parcel
makeParcel model =
    ( { cached = model.cached
      , cachedOnly = model.cachedOnly
      , enabledSourceIds = model.enabledSourceIds
      , favourites = model.favourites
      , favouritesOnly = model.favouritesOnly
      , grouping = model.grouping
      , hideDuplicates = model.hideDuplicates
      , searchResults = model.searchResults
      , selectedPlaylist = model.selectedPlaylist
      , sortBy = model.sortBy
      , sortDirection = model.sortDirection
      }
    , model.collection
    )


resolveParcel : Model -> Parcel -> Return Model Msg UI.Reply
resolveParcel model ( _, newCollection ) =
    let
        scrollObj =
            Json.Encode.object
                [ ( "scrollTop", Json.Encode.int 0 ) ]

        scrollEvent =
            Json.Encode.object
                [ ( "target", scrollObj ) ]

        collectionChanged =
            Collection.tracksChanged
                model.collection.untouched
                newCollection.untouched

        harvestChanged =
            if collectionChanged then
                True

            else
                Collection.harvestChanged
                    model.collection.harvested
                    newCollection.harvested

        listSceneModel =
            model.listScene

        listScene =
            if harvestChanged && model.scene == List then
                { listSceneModel | infiniteList = InfiniteList.updateScroll scrollEvent listSceneModel.infiniteList }

            else
                listSceneModel

        modelWithNewCollection =
            { model
                | collection = newCollection
                , listScene = listScene
                , selectedTrackIndexes =
                    if harvestChanged then
                        []

                    else
                        model.selectedTrackIndexes
            }
    in
    ( modelWithNewCollection
      ----------
      -- Command
      ----------
    , if harvestChanged then
        case model.scene of
            List ->
                Cmd.map ListSceneMsg UI.Tracks.Scene.List.scrollToTop

      else
        Cmd.none
      --------
      -- Reply
      --------
    , if collectionChanged then
        [ GenerateDirectoryPlaylists, ResetQueue ]

      else if harvestChanged then
        [ ResetQueue ]

      else
        []
    )


reviseCollection : (Parcel -> Parcel) -> Model -> Return Model Msg UI.Reply
reviseCollection collector model =
    model
        |> makeParcel
        |> collector
        |> resolveParcel model



-- 📣  ░░  FAVOURITES


toggleFavourite : Model -> IdentifiedTrack -> Return Model Msg UI.Reply
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
        |> addReply SaveFavourites



-- 🗺


type alias Dependencies =
    { amountOfSources : Int
    , bgColor : Maybe Color
    , isOnIndexPage : Bool
    , sourceIdsBeingProcessed : List String
    , viewport : Viewport
    }


view : Model -> Dependencies -> Html Msg
view model deps =
    chunk
        viewClasses
        [ lazy6
            navigation
            model.grouping
            model.favouritesOnly
            model.searchTerm
            model.selectedPlaylist
            deps.isOnIndexPage
            deps.bgColor

        --
        , if List.isEmpty model.collection.harvested then
            lazy4
                noTracksView
                deps.sourceIdsBeingProcessed
                deps.amountOfSources
                (List.length model.collection.harvested)
                (List.length model.favourites)

          else
            case model.scene of
                List ->
                    listView model deps
        ]


viewClasses : List String
viewClasses =
    [ T.flex
    , T.flex_column
    , T.flex_grow_1
    ]


navigation : Maybe Grouping -> Bool -> Maybe String -> Maybe Playlist -> Bool -> Maybe Color -> Html Msg
navigation maybeGrouping favouritesOnly searchTerm selectedPlaylist isOnIndexPage bgColor =
    let
        tabindex_ =
            ifThenElse isOnIndexPage 0 -1
    in
    chunk
        [ T.flex ]
        [ -----------------------------------------
          -- Part 1
          -----------------------------------------
          brick
            [ css searchStyles ]
            [ T.flex
            , T.flex_grow_1
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
                , T.color_inherit
                , T.flex_grow_1
                , T.h_100
                , T.outline_0
                , T.pr2
                , T.w_100
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
                [ Html.fromUnstyled (Icons.search 16 searchIconColoring) ]

            -- Actions
            ----------
            , brick
                [ css searchActionsStyles ]
                [ T.flex
                , T.items_center
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
                            [ Html.fromUnstyled (Icons.clear 16 searchIconColoring) ]

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
                            Html.fromUnstyled (Icons.favorite 16 <| Color UI.Kit.colorKit.base08)

                        False ->
                            Html.fromUnstyled (Icons.favorite_border 16 searchIconColoring)
                    ]

                -- 3
                , brick
                    [ css searchActionIconStyle
                    , fromUnstyled (Mouse.onClick <| ShowViewMenu maybeGrouping)
                    , title "View settings"
                    ]
                    [ T.pointer ]
                    [ Html.fromUnstyled (Icons.more_vert 16 searchIconColoring) ]

                -- 4
                , case selectedPlaylist of
                    Just playlist ->
                        brick
                            [ css (selectedPlaylistStyles bgColor)
                            , onClick DeselectPlaylist
                            ]
                            [ T.br2
                            , T.f7
                            , T.fw7
                            , T.lh_solid
                            , T.pointer
                            , T.truncate
                            , T.white_90
                            ]
                            [ text playlist.name ]

                    Nothing ->
                        nothing
                ]
            ]
        , -----------------------------------------
          -- Part 2
          -----------------------------------------
          UI.Navigation.localWithTabindex
            tabindex_
            [ ( Icon Icons.waves
              , Label "Playlists" Hidden
              , NavigateToPage (UI.Page.Playlists UI.Playlists.Page.Index)
              )
            , ( Icon Icons.schedule
              , Label "Queue" Hidden
              , NavigateToPage (UI.Page.Queue UI.Queue.Page.Index)
              )
            , ( Icon Icons.equalizer
              , Label "Equalizer" Hidden
              , NavigateToPage UI.Page.Equalizer
              )
            ]
        ]


noTracksView : List String -> Int -> Int -> Int -> Html Msg
noTracksView isProcessing amountOfSources amountOfTracks amountOfFavourites =
    chunk
        [ T.flex, T.flex_grow_1 ]
        [ UI.Kit.centeredContent
            [ if List.length isProcessing > 0 then
                message "Processing Tracks"

              else if amountOfSources == 0 then
                chunk
                    [ T.flex, T.items_start, T.ph3 ]
                    [ -- Add
                      ------
                      inline
                        [ T.dib, T.mb2 ]
                        [ UI.Kit.buttonLink
                            (Sources.NewOnboarding
                                |> UI.Page.Sources
                                |> UI.Page.toString
                            )
                            UI.Kit.Filled
                            (inline
                                []
                                [ UI.Kit.inlineIcon Icons.add
                                , text "Add some music"
                                ]
                            )
                        ]

                    -- Demo
                    -------
                    , slab
                        Html.span
                        []
                        [ T.dib, T.w1 ]
                        []
                    , UI.Kit.buttonWithColor
                        UI.Kit.colorKit.base04
                        UI.Kit.Normal
                        (Reply InsertDemo)
                        (inline
                            []
                            [ UI.Kit.inlineIcon Icons.music_note
                            , text "Insert demo"
                            ]
                        )

                    -- How
                    ------
                    , slab
                        Html.span
                        []
                        [ T.dib, T.w1 ]
                        []
                    , UI.Kit.buttonWithOptions
                        Html.a
                        [ href "about"
                        , target "_blank"
                        ]
                        UI.Kit.colorKit.base04
                        UI.Kit.Normal
                        Nothing
                        (inline
                            []
                            [ UI.Kit.inlineIcon Icons.help
                            , text "More info"
                            ]
                        )
                    ]

              else if amountOfTracks == 0 then
                message "No tracks found"

              else
                message "No sources available"
            ]
        ]


message : String -> Html Msg
message m =
    chunk
        [ T.bb, T.bw1, T.f6, T.fw6, T.lh_title, T.pb1 ]
        [ text m ]


listView : Model -> Dependencies -> Html Msg
listView model deps =
    model.selectedPlaylist
        |> Maybe.map .autoGenerated
        |> Maybe.andThen
            (\bool ->
                if bool then
                    Nothing

                else
                    Just model.listScene.dnd
            )
        |> UI.Tracks.Scene.List.view
            { height = deps.viewport.height
            , isVisible = deps.isOnIndexPage
            , showAlbum = deps.viewport.width >= 720
            }
            model.collection.harvested
            model.listScene.infiniteList
            model.favouritesOnly
            model.nowPlaying
            model.searchTerm
            model.sortBy
            model.sortDirection
            model.selectedTrackIndexes
        |> Html.map ListSceneMsg



-- 🖼


searchStyles : List Css.Style
searchStyles =
    [ Css.borderBottom3 (Css.px 1) Css.solid (Color.toElmCssColor UI.Kit.colors.subtleBorder)
    , Css.borderRight3 (Css.px 1) Css.solid (Color.toElmCssColor UI.Kit.colors.subtleBorder)
    ]


searchActionsStyles : List Css.Style
searchActionsStyles =
    [ Css.fontSize (Css.px 0)
    , Css.lineHeight (Css.px 0)
    , Css.paddingRight (Css.px <| 13 - 6)
    ]


searchActionIconStyle : List Css.Style
searchActionIconStyle =
    [ Css.height (Css.px 15)
    , Css.marginRight (Css.px 6)
    ]


searchIconColoring : Coloring
searchIconColoring =
    Color (Color.rgb255 205 205 205)


searchIconStyles : List Css.Style
searchIconStyles =
    [ Css.marginTop (Css.px 1)
    , Css.paddingLeft (Css.px 13)
    ]


searchInputStyles : List Css.Style
searchInputStyles =
    [ Css.fontSize (Css.px 14)
    , Css.height (Css.pct 98)
    , Css.minWidth (Css.px 59)
    , Css.paddingLeft (Css.px <| 13 + 16 + 9)
    ]


selectedPlaylistStyles : Maybe Color -> List Css.Style
selectedPlaylistStyles bgColor =
    [ Css.backgroundColor (Color.toElmCssColor <| Maybe.withDefault UI.Kit.colorKit.base01 bgColor)
    , Css.fontSize (Css.px 11)
    , Css.marginRight (Css.px 6)
    , Css.padding3 (Css.px 5) (Css.px 5.5) (Css.px 4)
    , Css.Transitions.transition [ Css.Transitions.backgroundColor 450 ]
    ]
