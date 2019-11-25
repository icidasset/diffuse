module UI.Tracks exposing (Dependencies, Model, Msg(..), Scene(..), importHypaethral, initialModel, update, view)

import Alien
import Chunky exposing (..)
import Color exposing (Color)
import Color.Ext as Color
import Common exposing (Switch(..))
import Conditional exposing (ifThenElse)
import Coordinates exposing (Coordinates, Viewport)
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (href, placeholder, style, tabindex, target, title, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Html.Events.Extra.Mouse as Mouse
import Html.Ext exposing (onEnterKey)
import Html.Lazy exposing (..)
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
import Sources
import Task.Extra as Task
import Tracks exposing (..)
import Tracks.Collection as Collection exposing (..)
import Tracks.Encoding as Encoding
import Tracks.Favourites as Favourites
import UI.DnD as DnD
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page
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
    [ C.flex
    , C.flex_col
    , C.flex_grow
    ]


navigation : Maybe Grouping -> Bool -> Maybe String -> Maybe Playlist -> Bool -> Maybe Color -> Html Msg
navigation maybeGrouping favouritesOnly searchTerm selectedPlaylist isOnIndexPage bgColor =
    let
        tabindex_ =
            ifThenElse isOnIndexPage 0 -1
    in
    chunk
        [ C.flex ]
        [ -----------------------------------------
          -- Part 1
          -----------------------------------------
          chunk
            [ C.border_b
            , C.border_r
            , C.border_subtle
            , C.flex
            , C.flex_grow
            , C.mt_px
            , C.overflow_hidden
            , C.relative
            ]
            [ -- Input
              --------
              slab
                Html.input
                [ onBlur Search
                , onEnterKey Search
                , onInput SetSearchTerm
                , placeholder "Search"
                , tabindex tabindex_
                , value (Maybe.withDefault "" searchTerm)
                ]
                [ C.bg_transparent
                , C.border_none
                , C.text_inherit
                , C.flex_grow
                , C.h_full
                , C.ml_1
                , C.mt_px
                , C.outline_none
                , C.pl_8
                , C.pr_2
                , C.pt_px
                , C.text_sm
                , C.w_full
                ]
                []

            -- Search icon
            --------------
            , chunk
                [ C.absolute
                , C.bottom_0
                , C.flex
                , C.items_center
                , C.left_0
                , C.ml_3
                , C.mt_px
                , C.top_0
                , C.z_0
                ]
                [ Icons.search 16 searchIconColoring ]

            -- Actions
            ----------
            , chunk
                [ C.flex
                , C.items_center
                , C.mr_3
                , C.mt_px
                , C.pt_px
                ]
                [ -- 1
                  case searchTerm of
                    Just _ ->
                        brick
                            [ onClick ClearSearch
                            , title "Clear search"
                            ]
                            [ C.cursor_pointer
                            , C.ml_1
                            , C.mt_px
                            ]
                            [ Icons.clear 16 searchIconColoring ]

                    Nothing ->
                        nothing

                -- 2
                , brick
                    [ onClick ToggleFavouritesOnly
                    , title "Toggle favourites-only"
                    ]
                    [ C.cursor_pointer
                    , C.ml_1
                    ]
                    [ case favouritesOnly of
                        True ->
                            Icons.favorite 16 (Color UI.Kit.colorKit.base08)

                        False ->
                            Icons.favorite_border 16 searchIconColoring
                    ]

                -- 3
                , brick
                    [ Mouse.onClick (ShowViewMenu maybeGrouping)
                    , title "View settings"
                    ]
                    [ C.cursor_pointer
                    , C.ml_1
                    ]
                    [ Icons.more_vert 16 searchIconColoring ]

                -- 4
                , case selectedPlaylist of
                    Just playlist ->
                        brick
                            [ onClick DeselectPlaylist

                            --
                            , bgColor
                                |> Maybe.withDefault UI.Kit.colorKit.base01
                                |> Color.toCssString
                                |> style "background-color"
                            ]
                            [ C.antialiased
                            , C.cursor_pointer
                            , C.font_bold
                            , C.leading_none
                            , C.ml_1
                            , C.px_1
                            , C.py_1
                            , C.rounded
                            , C.truncate
                            , C.text_white_90
                            , C.text_xxs
                            , C.transition_500
                            ]
                            [ chunk
                                [ C.px_px, C.pt_px ]
                                [ text playlist.name ]
                            ]

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
        [ C.flex, C.flex_grow ]
        [ UI.Kit.centeredContent
            [ if List.length isProcessing > 0 then
                message "Processing Tracks"

              else if amountOfSources == 0 then
                chunk
                    [ C.flex
                    , C.flex_wrap
                    , C.items_start
                    , C.justify_center
                    , C.px_3
                    ]
                    [ -- Add
                      ------
                      inline
                        [ C.mb_3, C.mx_2, C.whitespace_no_wrap ]
                        [ UI.Kit.buttonLink
                            (Sources.NewOnboarding
                                |> UI.Page.Sources
                                |> UI.Page.toString
                            )
                            UI.Kit.Filled
                            (buttonContents
                                [ UI.Kit.inlineIcon Icons.add
                                , text "Add some music"
                                ]
                            )
                        ]

                    -- Demo
                    -------
                    , inline
                        [ C.mb_3, C.mx_2, C.whitespace_no_wrap ]
                        [ UI.Kit.buttonWithColor
                            UI.Kit.Gray
                            UI.Kit.Normal
                            (Reply InsertDemo)
                            (buttonContents
                                [ UI.Kit.inlineIcon Icons.music_note
                                , text "Insert demo"
                                ]
                            )
                        ]

                    -- How
                    ------
                    , inline
                        [ C.mb_3, C.mx_2, C.whitespace_no_wrap ]
                        [ UI.Kit.buttonWithOptions
                            Html.a
                            [ href "about"
                            , target "_blank"
                            ]
                            UI.Kit.Gray
                            UI.Kit.Normal
                            Nothing
                            (buttonContents
                                [ UI.Kit.inlineIcon Icons.help
                                , text "More info"
                                ]
                            )
                        ]
                    ]

              else if amountOfTracks == 0 then
                message "No tracks found"

              else
                message "No sources available"
            ]
        ]


buttonContents : List (Html Msg) -> Html Msg
buttonContents =
    slab
        Html.span
        [ style "height" "21px" ]
        [ C.flex
        , C.items_center
        , C.leading_0
        , C.mt_px
        , C.pt_1
        ]


message : String -> Html Msg
message m =
    chunk
        [ C.border_b_2
        , C.border_current_color
        , C.text_sm
        , C.font_semibold
        , C.leading_snug
        , C.pb_1
        ]
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
            { bgColor = deps.bgColor
            , height = deps.viewport.height
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


searchIconColoring : Coloring
searchIconColoring =
    Color (Color.rgb255 198 198 198)
