module Tracks.State exposing (..)

import Dom.Scroll
import Json.Encode as Json
import List.Extra as List
import Playlists.Types exposing (Playlist)
import Response
import Response.Ext as Response exposing (..)
import Routing.Types
import Task
import Time
import Tracks.Collection as Collection exposing (..)
import Tracks.Encoding
import Tracks.Favourites as Favourites
import Tracks.Ports as Ports
import Tracks.Types exposing (..)
import Tracks.Utils exposing (..)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { activeTrackId = Nothing
    , collection = emptyCollection
    , enabledSourceIds = []
    , exposedStep = 1
    , favourites = []
    , favouritesOnly = False
    , searchResults = Nothing
    , searchTerm = Nothing
    , selectedPlaylist = Nothing
    , sortBy = Artist
    , sortDirection = Asc
    }



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        -- # Recalibrate
        --
        Recalibrate ->
            model
                |> Collection.makeParcel
                |> Collection.recalibrate
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
                    |> Collection.reidentify
                    |> Collection.set

        ------------------------------------
        -- Collection, Pt. 1
        ------------------------------------
        -- # Initial Collection
        --
        InitialCollection parcel ->
            parcel
                |> Collection.reidentify
                |> Collection.setWithoutConsequences
                |> Response.andAlso search

        ------------------------------------
        -- Collection, Pt. 2
        ------------------------------------
        -- # Add
        -- > Add tracks to the collection.
        --
        Add additionalTracks ->
            model
                |> Collection.makeParcel
                |> Collection.recalibrate
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

        -- > Step 2, perform search
        Search (Just term) ->
            { model | searchTerm = Just term }
                |> Response.withCmd (Ports.performSearch term)
                |> Response.andAlso storeUserData

        Search Nothing ->
            { model | searchResults = Nothing, searchTerm = Nothing }
                |> Collection.makeParcel
                |> Collection.recalibrate
                |> Collection.reharvest
                |> Collection.set
                |> Response.andAlso storeUserData

        -- > Step 3, receive search results
        ReceiveSearchResults trackIds ->
            { model | searchResults = Just trackIds }
                |> Collection.makeParcel
                |> Collection.recalibrate
                |> Collection.reharvest
                |> Collection.set

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
                |> Collection.recalibrate
                |> Collection.reharvest
                |> Collection.set
                |> Response.andAlso storeUserData

        ------------------------------------
        -- Playlists
        ------------------------------------
        -- > Remove selected playlist
        TogglePlaylist playlist ->
            { model | selectedPlaylist = togglePlaylist model playlist }
                |> Collection.makeParcel
                |> Collection.reharvest
                |> Collection.set
                |> Response.andAlso storeUserData
                |> Response.andAlso gotoIndexPage

        ------------------------------------
        -- UI
        ------------------------------------
        -- > Identify the active track
        --
        SetActiveTrackId maybeTrack ->
            let
                mapFn =
                    case maybeTrack of
                        Just track ->
                            \( i, t ) -> ( { i | isNowPlaying = t == track }, t )

                        Nothing ->
                            \( i, t ) -> ( { i | isNowPlaying = False }, t )
            in
                { model | activeTrackId = Maybe.map .id maybeTrack }
                    |> Collection.makeParcel
                    |> Collection.remap (List.map mapFn)
                    |> Collection.set

        -- > Table-scroll event handler
        --
        ScrollThroughTable { scrolledHeight, contentHeight, containerHeight } ->
            case scrolledHeight >= (contentHeight - containerHeight - 50) of
                True ->
                    { model | exposedStep = model.exposedStep + 1 }
                        |> Collection.makeParcel
                        |> Collection.reexpose
                        |> Collection.set

                False ->
                    Response.withNone model

        -- > Scroll to the active track
        --
        ScrollToActiveTrack track ->
            model.collection.harvested
                |> List.findIndex (Tuple.second >> (==) track)
                |> Maybe.map (scrollToIndex model)
                |> Maybe.withDefault (Response.withNone model)



-- 🔥 / Functions


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
            |> addCmd (do TopLevel.StoreUserData)


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
            (7 + 5 + idx * 32)
                |> toFloat
                |> Dom.Scroll.toY "tracks"

        exposedCmd =
            if not isExposed then
                do (TopLevel.TracksMsg Reharvest)
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



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveSearchResults ReceiveSearchResults ]



-- Utils


gotoIndexPage : Model -> Cmd TopLevel.Msg
gotoIndexPage model =
    Routing.Types.Index
        |> Routing.Types.GoToPage
        |> TopLevel.RoutingMsg
        |> do


storeUserData : Model -> Cmd TopLevel.Msg
storeUserData _ =
    do TopLevel.DebounceStoreUserData


search : Model -> Cmd TopLevel.Msg
search model =
    model.searchTerm
        |> Search
        |> TopLevel.TracksMsg
        |> do
