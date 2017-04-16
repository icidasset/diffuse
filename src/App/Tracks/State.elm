module Tracks.State exposing (..)

import ElmTextSearch
import List.Extra as List
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

        index =
            createIndex
                |> ElmTextSearch.addDocs collection
                |> Tuple.first
    in
        { collection = collection
        , filteredCollection = collection
        , index = index
        , indexFor = ""
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
                ($)
                    { model | collection = col }
                    [ do UpdateSearchIndex ]
                    [ do TopLevel.CleanQueue, storeTracks col ]

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
                ($)
                    { model | collection = col }
                    [ do UpdateSearchIndex ]
                    [ do TopLevel.CleanQueue, storeTracks col ]

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
                    { model | collection = col }
                    [ do UpdateSearchIndex ]
                    [ do TopLevel.CleanQueue, storeTracks col ]

        -- # Search
        --
        Search ->
            case model.searchTerm of
                Just term ->
                    search model term

                Nothing ->
                    (!) { model | filteredCollection = model.collection } []

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

        UpdateSearchIndex ->
            ($)
                { model
                    | index =
                        createIndex
                            |> ElmTextSearch.addDocs model.collection
                            |> Tuple.first
                }
                [ do Search ]
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
                (!)
                    { model
                        | collection = sortTracksBy property sortDir model.collection
                        , sortBy = property
                        , sortDirection = sortDir
                    }
                    []



-- Utils


{-| Search.
-}
search : Model -> String -> ( Model, Cmd TopLevel.Msg )
search model filter =
    let
        searchResults =
            model.index
                |> ElmTextSearch.search filter
                |> Result.toMaybe
    in
        case searchResults of
            Just ( newIndex, results ) ->
                (!)
                    { model
                        | filteredCollection =
                            let
                                trackIds =
                                    List.map Tuple.first results
                            in
                                List.filter
                                    (.id >> (\t -> List.member t trackIds))
                                    (model.collection)
                        , index = newIndex
                        , indexFor = filter
                    }
                    []

            Nothing ->
                (!)
                    model
                    []


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
