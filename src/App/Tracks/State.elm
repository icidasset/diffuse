module Tracks.State exposing (..)

import Firebase.Data
import Json.Encode as Json
import List.Extra as List
import Response
import Time
import Tracks.Collection as Collection exposing (..)
import Tracks.Encoding
import Tracks.Favourites as Favourites
import Tracks.Ports as Ports
import Tracks.Types exposing (..)
import Tracks.Utils exposing (..)
import Types as TopLevel
import Utils exposing (addCmd, do, doDelayed)


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    { collection = emptyCollection
    , exposedStep = 1
    , favourites = decodeFavourites (Maybe.withDefault [] flags.favourites)
    , favouritesOnly = False
    , searchResults = Nothing
    , searchTerm = Nothing
    , sortBy = Artist
    , sortDirection = Asc
    }


initialCommands : Maybe (List Json.Value) -> Cmd TopLevel.Msg
initialCommands maybeEncodedTracks =
    let
        encodedTracks =
            Maybe.withDefault [] maybeEncodedTracks
    in
        Cmd.batch
            [ -- Don't block the UI
              encodedTracks
                |> InitialCollection
                |> TopLevel.TracksMsg
                |> doDelayed (Time.millisecond * 250)

            -- Fill queue
            , TopLevel.FillQueue
                |> doDelayed (Time.millisecond * 500)

            -- Hide loader
            , TopLevel.HideLoadingScreen
                |> doDelayed (Time.millisecond * 500)
            ]



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
        InitialCollection encodedTracks ->
            model
                |> Collection.makeParcel
                |> Collection.add (decodeTracks encodedTracks)
                |> Collection.setWithoutConsequences

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
                |> Response.withNone

        SetSearchTerm value ->
            { model | searchTerm = Just value }
                |> Response.withNone

        -- > Step 2, perform search
        Search (Just term) ->
            { model | searchTerm = Just term }
                |> Response.withCmd (Ports.performSearch term)

        Search Nothing ->
            { model | searchResults = Nothing, searchTerm = Nothing }
                |> Collection.makeParcel
                |> Collection.recalibrate
                |> Collection.reharvest
                |> Collection.set

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

        -- Filter collection by favourites only {toggle}
        ToggleFavouritesOnly ->
            { model | favouritesOnly = not model.favouritesOnly }
                |> Collection.makeParcel
                |> Collection.recalibrate
                |> Collection.reharvest
                |> Collection.set

        ------------------------------------
        -- UI
        ------------------------------------
        ScrollThroughTable { scrolledHeight, contentHeight, containerHeight } ->
            case scrolledHeight >= (contentHeight - containerHeight - 50) of
                True ->
                    { model | exposedStep = model.exposedStep + 1 }
                        |> Collection.makeParcel
                        |> Collection.reexpose
                        |> Collection.set

                False ->
                    Response.withNone model


toggleFavourite : Model -> IdentifiedTrack -> ( Model, Cmd TopLevel.Msg )
toggleFavourite model ( i, t ) =
    let
        newFavourites =
            Favourites.toggleInList model.favourites ( i, t )

        storeFavourites =
            newFavourites
                |> List.map Tracks.Encoding.encodeFavourite
                |> Firebase.Data.storeFavourites

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
            |> addCmd storeFavourites



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveSearchResults ReceiveSearchResults ]
