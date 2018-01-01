module Tracks.State exposing (..)

import Debounce
import Dom.Scroll
import List.Extra as List
import Playlists.Types exposing (Playlist)
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
    , enabledSourceIds = []
    , exposedStep = 1
    , favourites = []
    , favouritesOnly = False
    , initialImportPerformed = False
    , scrollDebounce = Debounce.init
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


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
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
            model
                |> Collection.makeParcel
                |> Collection.recalibrate
                |> Collection.reexpose
                |> Collection.set
                |> Response.mapModel (\m -> { m | selectedTrackIndexes = [] })
                |> Response.addCmd recalibrationEffect

        -- # Reexpose
        --
        Reexpose ->
            model
                |> Collection.makeParcel
                |> Collection.reexpose
                |> Collection.set

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
                |> Maybe.andThen (\idx -> List.getAt idx model.collection.exposed)
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
                    |> Collection.reexpose
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
                    |> Collection.reexpose
                    |> Collection.set

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

        -- > Table-scroll event handler
        --
        ScrollThroughTable { scrolledHeight, contentHeight, containerHeight } ->
            case scrolledHeight >= (contentHeight - containerHeight - trackHeight * 25) of
                True ->
                    { model | exposedStep = model.exposedStep + 1 }
                        |> Collection.makeParcel
                        |> Collection.reexpose
                        |> Collection.set

                False ->
                    Response.withNone model

        ScrollThroughTableDebounced scrollPos ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceScrollConfig scrollPos model.scrollDebounce
            in
                ($)
                    { model | scrollDebounce = debounce }
                    [ cmd ]
                    []

        ScrollThroughTableDebounceCallback debounceMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceScrollConfig
                        (Debounce.takeLast (ScrollThroughTable >> do))
                        debounceMsg
                        model.scrollDebounce
            in
                ($)
                    { model | scrollDebounce = debounce }
                    [ cmd ]
                    []

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
    let
        isExposed =
            (idx + 1) <= List.length model.collection.exposed

        newExposedStep =
            if not isExposed then
                ceiling (toFloat idx / toFloat Collection.partial)
            else
                model.exposedStep

        scrollTask =
            (9 + idx * trackHeight)
                |> toFloat
                |> Dom.Scroll.toY "tracks"

        exposedCmd =
            if not isExposed then
                do (TopLevel.TracksMsg Reexpose)
            else
                Cmd.none

        cmd =
            Cmd.batch
                [ exposedCmd
                , Task.attempt (always TopLevel.NoOp) scrollTask
                ]
    in
        { model | exposedStep = newExposedStep }
            |> Response.withCmd cmd



-- 🔥 / Debounce configurations


debounceScrollConfig : Debounce.Config Msg
debounceScrollConfig =
    { strategy = Debounce.soon (125 * Time.millisecond)
    , transform = ScrollThroughTableDebounceCallback
    }


debounceSearchConfig : Debounce.Config Msg
debounceSearchConfig =
    { strategy = Debounce.later (250 * Time.millisecond)
    , transform = DebouncedSearchCallback
    }



-- 🌱


subscriptions : Model -> Sub Msg
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
