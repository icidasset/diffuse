module Tracks.State exposing (..)

import List.Extra as List
import Tracks.Ports as Ports
import Tracks.Types exposing (..)
import Tracks.Utils exposing (..)
import Types as TopLevel
import Utils exposing (do)


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    let
        collection =
            decodeTracks flags
    in
        { collection = collection
        , resultant = List.take partial collection
        , searchResults = collection
        , searchTerm = Nothing
        , sortBy = Artist
        , sortDirection = Asc
        }


initialCommands : Cmd TopLevel.Msg
initialCommands =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        -- # Add
        -- > Add tracks to the collection.
        --
        Add additionalTracks ->
            let
                col =
                    additionalTracks
                        |> List.append model.collection
                        |> sortTracksBy model.sortBy model.sortDirection
            in
                (!)
                    { model | collection = col }
                    [ do TopLevel.CleanQueue, handleNewCollection col ]

        -- # Remove
        -- > Remove tracks from the collection,
        --   matching by the `sourceId`.
        --
        Remove sourceId ->
            let
                col =
                    List.filter
                        (\t -> t.sourceId /= sourceId)
                        model.collection
            in
                (!)
                    { model | collection = col }
                    [ do TopLevel.CleanQueue, handleNewCollection col ]

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
                (!)
                    { model | collection = col }
                    [ do TopLevel.CleanQueue, handleNewCollection col ]

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

        -- # Search
        --
        ReceiveSearchResults trackIds ->
            let
                col =
                    List.filter (\t -> List.member t.id trackIds) model.collection
            in
                ($)
                    { model | searchResults = col }
                    [ do Recalibrate ]
                    []

        Search ->
            case model.searchTerm of
                Just term ->
                    (!)
                        model
                        [ Ports.performSearch term ]

                Nothing ->
                    ($)
                        { model | searchResults = model.collection }
                        [ do Recalibrate ]
                        []

        SetSearchTerm value ->
            let
                searchTerm =
                    case String.trim value of
                        "" ->
                            Nothing

                        v ->
                            Just v
            in
                (!)
                    { model | searchTerm = searchTerm }
                    []

        -- # UI
        --
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
