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
    , collectionIdentified = []
    , collectionHarvested = []
    , collectionExposed = []
    , exposedStep = 0
    , favourites = decodeFavourites (Maybe.withDefault [] flags.favourites)
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
              -- Pt. 1
              encodedTracks
                |> List.take partial
                |> InitialCollection
                |> TopLevel.TracksMsg
                |> do

            -- Pt.  2
            , encodedTracks
                |> List.drop partial
                |> InitialCollection
                |> TopLevel.TracksMsg
                |> doDelayed (Time.millisecond * 1000)

            -- Fill queue
            , TopLevel.FillQueue
                |> doDelayed (Time.millisecond * 1250)
            ]



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        -- # Recalibrate
        --
        Recalibrate ->
            (!) (expose 1 model) []

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

                sortFn =
                    sortTracksBy property sortDir
            in
                ($)
                    { model
                        | collectionIdentified = sortFn model.collectionIdentified
                        , collectionHarvested = sortFn model.collectionHarvested
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

                colIdent =
                    col
                        |> identifyCollection model.favourites
                        |> sortTracksBy model.sortBy model.sortDirection

                newModel =
                    { model
                        | collection = col
                        , collectionIdentified = colIdent
                        , collectionHarvested = colIdent
                    }
            in
                (!)
                    (expose 1 newModel)
                    []

        -- # Update Collection
        --
        UpdateCollection tracks ->
            let
                newIdentifiedCollection =
                    tracks
                        |> identifyCollection model.favourites
                        |> sortTracksBy model.sortBy model.sortDirection

                encodedCollection =
                    List.map Tracks.Encoding.encodeTrack tracks
            in
                (!)
                    { model
                        | collection = tracks
                        , collectionIdentified = newIdentifiedCollection
                    }
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
                        (.sourceId >> (/=) sourceId)
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
                { model | collectionHarvested = model.collectionIdentified, searchTerm = Nothing }
                [ do Recalibrate ]
                [ do TopLevel.ResetQueue ]

        Search (Just term) ->
            ($)
                { model | searchTerm = Just term }
                [ Ports.performSearch term ]
                []

        -- > Step 3, receive search results
        ReceiveSearchResults [] ->
            ($)
                { model | collectionHarvested = [] }
                [ do Recalibrate ]
                [ do TopLevel.ResetQueue ]

        ReceiveSearchResults trackIds ->
            ($)
                { model | collectionHarvested = harvest trackIds model }
                [ do Recalibrate ]
                [ do TopLevel.ResetQueue ]

        ------------------------------------
        -- Favourites
        ------------------------------------
        ToggleFavourite index_as_string ->
            let
                maybeIdentifiedTrack =
                    index_as_string
                        |> String.toInt
                        |> Result.toMaybe
                        |> Maybe.andThen (\idx -> List.getAt idx model.collectionExposed)
            in
                case maybeIdentifiedTrack of
                    Just ( i, t ) ->
                        let
                            newFavourites =
                                toggleFavourite model.favourites ( i, t )

                            encodedFavourites =
                                List.map Tracks.Encoding.encodeFavourite newFavourites
                        in
                            (!)
                                { model
                                    | collectionExposed =
                                        toggleFavouriteInCollection model.collectionIdentified t
                                    , collectionHarvested =
                                        toggleFavouriteInCollection model.collectionHarvested t
                                    , collectionExposed =
                                        toggleFavouriteInCollection model.collectionExposed t
                                    , favourites =
                                        newFavourites
                                }
                                [ Firebase.Data.storeFavourites encodedFavourites ]

                    Nothing ->
                        (!)
                            model
                            []

        ------------------------------------
        -- UI
        ------------------------------------
        ScrollThroughTable { scrolledHeight, contentHeight, containerHeight } ->
            if scrolledHeight >= (contentHeight - containerHeight - 50) then
                (!) (expose (model.exposedStep + 1) model) []
            else
                (!) (model) []



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveSearchResults ReceiveSearchResults ]



-- Sorting


sortTracksBy : SortBy -> SortDirection -> List IdentifiedTrack -> List IdentifiedTrack
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


sortByAlbum : IdentifiedTrack -> String
sortByAlbum twi =
    let
        t =
            Tuple.second twi
    in
        t.tags.title
            |> String.append (toString t.tags.nr)
            |> String.append t.tags.artist
            |> String.append t.tags.album
            |> String.toLower


sortByArtist : IdentifiedTrack -> String
sortByArtist twi =
    let
        t =
            Tuple.second twi
    in
        t.tags.title
            |> String.append (toString t.tags.nr)
            |> String.append t.tags.album
            |> String.append t.tags.artist
            |> String.toLower


sortByTitle : IdentifiedTrack -> String
sortByTitle twi =
    let
        t =
            Tuple.second twi
    in
        t.tags.album
            |> String.append t.tags.artist
            |> String.append t.tags.title
            |> String.toLower



-- Favouriting


toggleFavourite : List Favourite -> IdentifiedTrack -> List Favourite
toggleFavourite favourites ( i, t ) =
    let
        artist =
            String.toLower t.tags.artist

        title =
            String.toLower t.tags.title
    in
        case i.isFavourite of
            True ->
                List.filter
                    (\f -> not (f.artist == artist && f.title == title))
                    favourites

            False ->
                List.append
                    favourites
                    [ { artist = artist
                      , title = title
                      }
                    ]


toggleFavouriteInCollection : List IdentifiedTrack -> Track -> List IdentifiedTrack
toggleFavouriteInCollection collection track =
    let
        indexer =
            Tuple.second >> .id >> (==) track.id

        updater =
            Tuple.mapFirst (\i -> { i | isFavourite = not i.isFavourite })
    in
        collection
            |> List.findIndex indexer
            |> Maybe.andThen (\idx -> List.updateAt idx updater collection)
            |> Maybe.withDefault collection



-- Identifying


identifyCollection : List Favourite -> List Track -> List IdentifiedTrack
identifyCollection favourites tracks =
    tracks
        |> List.foldl identifier ( [], favourites )
        |> Tuple.first


identifier :
    Track
    -> ( List IdentifiedTrack, List Favourite )
    -> ( List IdentifiedTrack, List Favourite )
identifier track ( acc, favourites ) =
    let
        artist =
            String.toLower track.tags.artist

        title =
            String.toLower track.tags.title

        idx =
            List.findIndex
                (\f -> f.artist == artist && f.title == title)
                favourites
    in
        case idx of
            Just i ->
                ( acc ++ [ ( { isFavourite = True }, track ) ]
                , List.removeAt i favourites
                )

            Nothing ->
                ( acc ++ [ ( { isFavourite = False }, track ) ]
                , favourites
                )



-- Harvesting


harvest : List TrackId -> Model -> List IdentifiedTrack
harvest trackIds model =
    model.collectionIdentified
        |> List.foldl harvester ( [], trackIds )
        |> Tuple.first


harvester :
    IdentifiedTrack
    -> ( List IdentifiedTrack, List TrackId )
    -> ( List IdentifiedTrack, List TrackId )
harvester ( i, t ) ( acc, trackIds ) =
    case List.findIndex ((==) t.id) trackIds of
        Just idx ->
            ( acc ++ [ ( i, t ) ]
            , List.removeAt idx trackIds
            )

        Nothing ->
            ( acc
            , trackIds
            )



-- Exposing


expose : Int -> Model -> Model
expose step model =
    { model
        | collectionExposed = List.take (step * partial) model.collectionHarvested
        , exposedStep = step
    }
