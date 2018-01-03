module Tracks.State exposing (..)

import Debounce
import Dom.Scroll
import DnD
import InfiniteList
import List.Extra as List
import List.Ext as List
import Playlists.Types exposing (Msg(UpdatePlaylist), Playlist)
import Response
import Response.Ext as Response exposing (..)
import Routing.Types
import Routing.Utils exposing (goTo)
import Task
import Time
import Tracks.Collection as Collection exposing (..)
import Tracks.Favourites as Favourites
import Tracks.Ports as Ports
import Tracks.Selecting as Selecting
import Tracks.Styles exposing (trackHeight)
import Tracks.Types exposing (..)
import Tracks.Utils exposing (..)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { activeIdentifiedTrack = Nothing
    , collection = emptyCollection
    , dnd = DnD.initial
    , enabledSourceIds = []
    , favourites = []
    , favouritesOnly = False
    , infiniteList = InfiniteList.init
    , initialImportPerformed = False
    , searchCounter = 0
    , searchDebounce = Debounce.init
    , searchResults = Nothing
    , searchTerm = Nothing
    , selectedPlaylist = Nothing
    , selectedTrackIndexes = []
    , sortBy = Artist
    , sortDirection = Asc
    }



-- 🔥


update : Tracks.Types.Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        -- # Rearrange
        --
        Rearrange ->
            model
                |> Collection.makeParcel
                |> Collection.rearrange
                |> Collection.set

        -- # Recalibrate
        --
        Recalibrate ->
            -1
                |> scrollToIndex model

        -- # Reharvest
        --
        Reharvest ->
            model
                |> Collection.makeParcel
                |> Collection.reharvest
                |> Collection.set

        -- # SetEnabledSourceIds
        --
        SetEnabledSourceIds ids ->
            model
                |> (\m -> { m | enabledSourceIds = ids })
                |> Collection.makeParcel
                |> Collection.reidentify
                |> Collection.set

        -- # Sort
        --
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
                    |> Collection.makeParcel
                    |> Collection.rearrange
                    |> Collection.set

        ------------------------------------
        -- Collection, Pt. 1
        ------------------------------------
        -- # Initial Collection
        --
        InitialCollection shouldProcessSources parcel ->
            parcel
                |> Collection.reidentify
                |> Collection.set
                |> Response.andAlso search
                |> Response.andAlso
                    (if shouldProcessSources then
                        processSources
                     else
                        always Cmd.none
                    )

        ------------------------------------
        -- Collection, Pt. 2
        ------------------------------------
        -- # Add
        -- > Add tracks to the collection.
        --
        Add additionalTracks ->
            model
                |> Collection.makeParcel
                |> Collection.add additionalTracks
                |> Collection.set

        -- # Remove
        -- > Remove tracks from the collection,
        --   matching by the `sourceId`.
        --
        Remove sourceId ->
            model
                |> Collection.makeParcel
                |> Collection.removeBySourceId sourceId
                |> Collection.set

        -- # Remove
        -- > Remove tracks from the collection,
        --   matching by the `sourceId` and the `path`.
        --
        RemoveByPath sourceId paths ->
            model
                |> Collection.makeParcel
                |> Collection.removeByPath sourceId paths
                |> Collection.set

        ------------------------------------
        -- Search
        ------------------------------------
        -- > Step 1, set search term
        SetSearchTerm "" ->
            { model | searchTerm = Nothing }
                |> Response.withAlso storeUserData

        SetSearchTerm value ->
            { model | searchTerm = Just value }
                |> Response.withAlso storeUserData

        ClearSearch ->
            update
                (Search Nothing)
                { model | searchCounter = model.searchCounter + 1 }

        -- > Step 2, perform search
        Search (Just term) ->
            { model | searchTerm = Just term }
                |> Response.withCmd (Ports.performSearch term)
                |> Response.andAlso storeUserData

        Search Nothing ->
            { model | searchResults = Nothing, searchTerm = Nothing }
                |> Collection.makeParcel
                |> Collection.reharvest
                |> Collection.set
                |> Response.andAlso storeUserData
                |> initialImport

        DebouncedSearch ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceSearchConfig () model.searchDebounce
            in
                ($)
                    { model | searchDebounce = debounce }
                    [ cmd ]
                    []

        DebouncedSearchCallback debounceMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceSearchConfig
                        (model.searchTerm
                            |> Search
                            |> do
                            |> always
                            |> Debounce.takeLast
                        )
                        debounceMsg
                        model.searchDebounce
            in
                ($)
                    { model | searchDebounce = debounce }
                    [ cmd ]
                    []

        -- > Step 3, receive search results
        ReceiveSearchResults trackIds ->
            { model | searchResults = Just trackIds }
                |> Collection.makeParcel
                |> Collection.reharvest
                |> Collection.set
                |> initialImport

        ------------------------------------
        -- Favourites
        ------------------------------------
        -- > Make a track a favourite, or remove it as a favourite
        ToggleFavourite index ->
            index
                |> String.toInt
                |> Result.toMaybe
                |> Maybe.andThen (\idx -> List.getAt idx model.collection.harvested)
                |> Maybe.map (toggleFavourite model)
                |> Maybe.withDefault ((,) model Cmd.none)

        -- > Filter collection by favourites only {toggle}
        ToggleFavouritesOnly ->
            { model | favouritesOnly = not model.favouritesOnly }
                |> Collection.makeParcel
                |> Collection.reharvest
                |> Collection.set
                |> Response.andAlso storeUserData

        ------------------------------------
        -- Playlists
        ------------------------------------
        -- > Toggle selected playlist
        --
        TogglePlaylist playlist ->
            { model | selectedPlaylist = togglePlaylist model playlist }
                |> Collection.makeParcel
                |> Collection.redoBasedOnPlaylist model.selectedPlaylist
                |> Collection.set
                |> Response.andAlso storeUserData
                |> Response.addCmd (goTo Routing.Types.Index)

        ------------------------------------
        -- UI
        ------------------------------------
        -- > Apply track selection
        --
        ApplyTrackSelection holdingShiftKey trackIndex ->
            let
                newSelection : List Int
                newSelection =
                    Selecting.inTable
                        { alreadySelected = model.selectedTrackIndexes
                        , holdingShiftKey = holdingShiftKey
                        , targetTrackIndex = trackIndex
                        }
            in
                { model | selectedTrackIndexes = newSelection }
                    |> Collection.makeParcel
                    |> Collection.redoSelection
                    |> Collection.set

        -- > Apply track selection using context menu
        --
        ApplyTrackSelectionUsingContextMenu trackIndex ->
            let
                newSelection : List Int
                newSelection =
                    Selecting.rightClickInTable
                        { alreadySelected = model.selectedTrackIndexes
                        , holdingShiftKey = False
                        , targetTrackIndex = trackIndex
                        }
            in
                { model | selectedTrackIndexes = newSelection }
                    |> Collection.makeParcel
                    |> Collection.redoSelection
                    |> Collection.set

        -- > Drag n' Drop
        --
        DnDMsg sub ->
            let
                dnd =
                    DnD.update sub model.dnd

                selectedPlaylist =
                    case dnd of
                        DnD.Dropped { origin, target } ->
                            case model.selectedPlaylist of
                                Just playlist ->
                                    playlist.tracks
                                        |> List.move { from = origin, to = target }
                                        |> (\t -> { playlist | tracks = t })
                                        |> Just

                                Nothing ->
                                    Nothing

                        _ ->
                            model.selectedPlaylist
            in
                (!)
                    { model | dnd = dnd, selectedPlaylist = selectedPlaylist }
                    [ if selectedPlaylist /= model.selectedPlaylist then
                        case selectedPlaylist of
                            Just playlist ->
                                Cmd.batch
                                    [ do (TopLevel.TracksMsg Rearrange)
                                    , do (TopLevel.PlaylistsMsg <| UpdatePlaylist playlist)
                                    ]

                            Nothing ->
                                Cmd.none
                      else
                        Cmd.none
                    ]

        -- > Infinite list
        --
        InfiniteListMsg infiniteList ->
            (!) { model | infiniteList = infiniteList } []

        -- > Identify the active track
        --
        SetActiveIdentifiedTrack maybeIdentifiedTrack ->
            let
                mapFn =
                    case maybeIdentifiedTrack of
                        Just a ->
                            \( i, t ) -> (,) { i | isNowPlaying = isNowPlaying a ( i, t ) } t

                        Nothing ->
                            \( i, t ) -> (,) { i | isNowPlaying = False } t
            in
                { model | activeIdentifiedTrack = maybeIdentifiedTrack }
                    |> Collection.makeParcel
                    |> Collection.remap (List.map mapFn)
                    |> Collection.set

        -- > Scroll to the active track
        --
        ScrollToActiveTrack track ->
            model.collection.harvested
                |> List.findIndex (Tuple.second >> (==) track)
                |> Maybe.map (scrollToIndex model)
                |> Maybe.withDefault (Response.withNone model)



-- 🔥 / Functions


initialImport : ( Model, Cmd TopLevel.Msg ) -> ( Model, Cmd TopLevel.Msg )
initialImport ( model, command ) =
    if model.initialImportPerformed == False then
        (!)
            { model | initialImportPerformed = True }
            [ command, do TopLevel.HideLoadingScreen ]
    else
        (,)
            model
            command


toggleFavourite : Model -> IdentifiedTrack -> ( Model, Cmd TopLevel.Msg )
toggleFavourite model ( i, t ) =
    let
        newFavourites =
            Favourites.toggleInList model.favourites ( i, t )

        effect =
            if model.favouritesOnly then
                remap (Favourites.toggleInCollection t) >> reharvest
            else
                remap (Favourites.toggleInCollection t)
    in
        { model | favourites = newFavourites }
            |> makeParcel
            |> effect
            |> set
            |> addCmd (do TopLevel.DebounceStoreUserData)


togglePlaylist : Model -> Playlist -> Maybe Playlist
togglePlaylist model playlist =
    case model.selectedPlaylist of
        Just selectedPlaylist ->
            if selectedPlaylist == playlist then
                Nothing
            else
                Just playlist

        Nothing ->
            Just playlist


scrollToIndex : Model -> Int -> ( Model, Cmd TopLevel.Msg )
scrollToIndex model idx =
    (!)
        model
        [ (9 + idx * trackHeight)
            |> toFloat
            |> Dom.Scroll.toY "tracks"
            |> Task.attempt (always TopLevel.NoOp)
        ]



-- 🔥 / Debounce configurations


debounceSearchConfig : Debounce.Config Tracks.Types.Msg
debounceSearchConfig =
    { strategy = Debounce.later (250 * Time.millisecond)
    , transform = DebouncedSearchCallback
    }



-- 🌱


subscriptions : Model -> Sub Tracks.Types.Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveSearchResults ReceiveSearchResults ]



-- Utilities


processSources : Model -> Cmd TopLevel.Msg
processSources _ =
    do TopLevel.ProcessSources


storeUserData : Model -> Cmd TopLevel.Msg
storeUserData model =
    if model.initialImportPerformed then
        do TopLevel.DebounceStoreUserData
    else
        Cmd.none


search : Model -> Cmd TopLevel.Msg
search model =
    model.searchTerm
        |> Search
        |> TopLevel.TracksMsg
        |> do
