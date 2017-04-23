module Tracks.State exposing (..)

import Firebase.Data
import Json.Encode as Json
import List.Extra as List
import Time
import Tracks.Encoding
import Tracks.Ports as Ports
import Tracks.Types exposing (..)
import Tracks.Utils exposing (..)
import Types as TopLevel
import Utils exposing (do, doDelayed)


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    { collection = []
    , resultant = []
    , searchResults = []
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
            [ encodedTracks
                |> List.take partial
                |> InitialCollection
                |> TopLevel.TracksMsg
                |> do
            , encodedTracks
                |> List.drop partial
                |> InitialCollection
                |> TopLevel.TracksMsg
                |> doDelayed (Time.millisecond * 1500)
            ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        -- # Recalibrate
        --
        Recalibrate ->
            let
                col =
                    List.take partial model.searchResults
            in
                (!)
                    { model | resultant = col }
                    []

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
                ($)
                    { model
                        | collection = sortTracksBy property sortDir model.collection
                        , searchResults = sortTracksBy property sortDir model.searchResults
                        , sortBy = property
                        , sortDirection = sortDir
                    }
                    [ do Recalibrate ]
                    []

        ------------------------------------
        -- Collection, Pt. 1
        ------------------------------------
        -- # Initial Collection
        --
        InitialCollection encodedTracks ->
            let
                col =
                    encodedTracks
                        |> decodeTracks
                        |> (++) model.collection
                        |> sortTracksBy model.sortBy model.sortDirection
            in
                (!)
                    { model
                        | collection = col
                        , resultant = List.take partial col
                        , searchResults = col
                    }
                    []

        -- # Update Collection
        --
        UpdateCollection tracks ->
            let
                newCollection =
                    sortTracksBy model.sortBy model.sortDirection tracks

                encodedCollection =
                    List.map Tracks.Encoding.encode newCollection
            in
                (!)
                    { model | collection = newCollection }
                    [ do TopLevel.CleanQueue
                    , Firebase.Data.storeTracks encodedCollection
                    , Ports.updateSearchIndex encodedCollection
                    , do (TopLevel.TracksMsg (Search model.searchTerm))
                    ]

        ------------------------------------
        -- Collection, Pt. 2
        ------------------------------------
        -- # Add
        -- > Add tracks to the collection.
        --
        Add additionalTracks ->
            let
                col =
                    model.collection ++ additionalTracks
            in
                ($)
                    model
                    [ do (UpdateCollection col) ]
                    []

        -- # Remove
        -- > Remove tracks from the collection,
        --   matching by the `sourceId`.
        --
        Remove sourceId ->
            let
                col =
                    List.filter
                        (\t -> t.sourceId /= sourceId)
                        (model.collection)
            in
                ($)
                    model
                    [ do (UpdateCollection col) ]
                    []

        -- # Remove
        -- > Remove tracks from the collection,
        --   matching by the `sourceId` and the `path`.
        --
        RemoveByPath sourceId pathsList ->
            let
                col =
                    List.filter
                        (\t ->
                            if t.sourceId == sourceId then
                                List.notMember t.path pathsList
                            else
                                True
                        )
                        model.collection
            in
                ($)
                    model
                    [ do (UpdateCollection col) ]
                    []

        ------------------------------------
        -- Search
        ------------------------------------
        -- > Step 1, set search term
        SetSearchTerm "" ->
            (!) { model | searchTerm = Nothing } []

        SetSearchTerm value ->
            (!) { model | searchTerm = Just value } []

        -- > Step 2, perform search
        Search Nothing ->
            ($)
                { model | searchTerm = Nothing, searchResults = model.collection }
                [ do Recalibrate ]
                []

        Search (Just term) ->
            ($)
                { model | searchTerm = Just term }
                [ Ports.performSearch term ]
                []

        -- > Step 3, receive search results
        ReceiveSearchResults [] ->
            ($)
                { model
                    | searchResults =
                        []
                }
                [ do Recalibrate ]
                []

        ReceiveSearchResults trackIds ->
            ($)
                { model
                    | searchResults =
                        List.filter (\t -> List.member t.id trackIds) model.collection
                }
                [ do Recalibrate ]
                []

        ------------------------------------
        -- UI
        ------------------------------------
        ScrollThroughTable { scrolledHeight, contentHeight, containerHeight } ->
            -- When you're over the point-of-no-return
            if scrolledHeight >= (contentHeight - containerHeight - 50) then
                let
                    par =
                        (List.length model.resultant) + partial

                    col =
                        List.take par model.searchResults
                in
                    (!) { model | resultant = col } []
            else
                (!) model []



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveSearchResults ReceiveSearchResults ]



-- Utils


{-| Sort.
-}
sortTracksBy : SortBy -> SortDirection -> List Track -> List Track
sortTracksBy property direction =
    let
        sortFn =
            case property of
                Album ->
                    sortByAlbum

                Artist ->
                    sortByArtist

                Title ->
                    sortByTitle

        dirFn =
            if direction == Desc then
                List.reverse
            else
                identity
    in
        List.sortBy sortFn >> dirFn


sortByAlbum : Track -> String
sortByAlbum t =
    t.tags.title
        |> String.append (toString t.tags.nr)
        |> String.append t.tags.artist
        |> String.append t.tags.album
        |> String.toLower


sortByArtist : Track -> String
sortByArtist t =
    t.tags.title
        |> String.append (toString t.tags.nr)
        |> String.append t.tags.album
        |> String.append t.tags.artist
        |> String.toLower


sortByTitle : Track -> String
sortByTitle t =
    t.tags.album
        |> String.append t.tags.artist
        |> String.append t.tags.title
        |> String.toLower
