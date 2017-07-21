module Sources.State exposing (..)

import Date
import Dict
import Http exposing (Error(..))
import List.Extra as List
import Maybe.Ext as Maybe
import Maybe.Extra as Maybe
import Navigation
import Response.Ext exposing (do)
import Sources.Ports as Ports
import Sources.Processing as Processing
import Sources.Types exposing (..)
import Sources.Utils exposing (..)
import Tracks.Types exposing (emptyTrack)
import Types as TopLevel


-- Services

import Sources.Services.AmazonS3 as AmazonS3
import Sources.Services.Ipfs as Ipfs


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    { collection = decodeSources flags
    , isProcessing = Nothing
    , newSource = initialSource
    , processingErrors = []
    , timestamp = Date.fromTime 0
    }


initialCommands : Cmd TopLevel.Msg
initialCommands =
    Cmd.none


initialSource : Source
initialSource =
    makeSource AmazonS3 AmazonS3.initialData



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        ------------------------------------
        -- Process
        ------------------------------------
        {- If already processing, do nothing.
           If there are no sources, do nothing.
           If there are sources, start processing the first source.
        -}
        Process tracks ->
            let
                processingData =
                    List.map
                        (\source ->
                            ( source
                            , List.filter (\t -> t.sourceId == source.id) tracks
                            )
                        )
                        model.collection

                isProcessing =
                    model.collection
                        |> List.head
                        |> Maybe.map (always processingData)
                        |> Maybe.preferFirst model.isProcessing

                command =
                    model.collection
                        |> List.head
                        |> Maybe.map (Processing.takeFirstStep model.timestamp)
                        |> Maybe.preferSecond (Maybe.map (always Cmd.none) model.isProcessing)
                        |> Maybe.withDefault Cmd.none

                processingErrors =
                    model.isProcessing
                        |> Maybe.map (\_ -> model.processingErrors)
                        |> Maybe.withDefault []
            in
                ($)
                    { model | isProcessing = isProcessing, processingErrors = processingErrors }
                    [ command ]
                    []

        {- If not processing, do nothing.
           If there are no sources left, do nothing.
           If there are sources left, start processing the next source in line.
        -}
        ProcessNextInLine ->
            let
                takeStep =
                    Processing.takeFirstStep model.timestamp

                maybe =
                    model.isProcessing
                        |> Maybe.andThen (List.tail)
                        |> Maybe.andThen (\a -> Maybe.map ((,) a) (List.head a))
                        |> Maybe.map (Tuple.mapSecond Tuple.first)
                        |> Maybe.map (Tuple.mapSecond takeStep)
            in
                ($)
                    { model | isProcessing = Maybe.map Tuple.first maybe }
                    [ maybe
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault Cmd.none
                    ]
                    []

        {- Processing step,
           Phase 1, `makeTree`.
           ie. make a file list/tree.
        -}
        ProcessTreeStep ctx (Ok resp) ->
            let
                associatedTracks =
                    model.isProcessing
                        |> Maybe.andThen List.head
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault []
            in
                ($)
                    model
                    [ Processing.takeTreeStep ctx resp associatedTracks model.timestamp ]
                    []

        ProcessTreeStep ctx (Err err) ->
            let
                publicError =
                    case err of
                        NetworkError ->
                            "Are you sure you're connected to cyberspace?!"

                        Timeout ->
                            "The server for this source type did not respond."

                        BadStatus response ->
                            Processing.decodeError ctx.source response.body

                        _ ->
                            toString err
            in
                ($)
                    { model
                        | processingErrors =
                            ( ctx.source.id, publicError ) :: model.processingErrors
                    }
                    [ do ProcessNextInLine ]
                    []

        ProcessTreeStepRemoveTracks sourceId filePaths ->
            ($)
                model
                []
                [ filePaths
                    |> Tracks.Types.RemoveByPath sourceId
                    |> TopLevel.TracksMsg
                    |> do
                ]

        {- Processing step,
           Phase 2, `makeTags`.
           ie. get the tags for each file in the file list.
        -}
        ProcessTagsStep tagsCtx ->
            let
                insert =
                    tagsCtx
                        |> Processing.tracksFromTagsContext
                        |> Tracks.Types.Add
                        |> TopLevel.TracksMsg
                        |> do

                cmd =
                    model.isProcessing
                        |> Maybe.map (List.map Tuple.first)
                        |> Maybe.andThen (Processing.findTagsContextSource tagsCtx)
                        |> Maybe.andThen (Processing.takeTagsStep model.timestamp tagsCtx)
                        |> Maybe.withDefault (do ProcessNextInLine)
            in
                ($) model [ cmd ] [ insert ]

        ------------------------------------
        -- CRUD
        ------------------------------------
        Destroy sourceId ->
            let
                newCollection =
                    List.filter (\s -> s.id /= sourceId) model.collection
            in
                ($)
                    { model | collection = newCollection }
                    []
                    [ sourceId
                        |> Tracks.Types.Remove
                        |> TopLevel.TracksMsg
                        |> do
                    , updateEnabledSourceIds newCollection
                    , storeSources newCollection
                    ]

        ------------------------------------
        -- Forms
        ------------------------------------
        SetNewSourceProperty source key value ->
            let
                newSource =
                    { source | data = Dict.insert key (String.trim value) source.data }
            in
                (!) { model | newSource = newSource } []

        SetNewSourceType typeString ->
            let
                newSource =
                    case typeString of
                        "AmazonS3" ->
                            makeSource AmazonS3 AmazonS3.initialData

                        "Ipfs" ->
                            makeSource Ipfs Ipfs.initialData

                        _ ->
                            initialSource
            in
                (!) { model | newSource = newSource } []

        SubmitNewSourceForm ->
            let
                newCollection =
                    (setProperSourceId model model.newSource) :: model.collection
            in
                ($)
                    { model
                        | collection = newCollection
                        , newSource = initialSource
                    }
                    []
                    [ do TopLevel.ProcessSources
                    , Navigation.newUrl "/"
                    , updateEnabledSourceIds newCollection
                    , storeSources newCollection
                    ]

        ------------------------------------
        -- Other
        ------------------------------------
        ToggleSource source ->
            let
                newCollection =
                    List.updateIf
                        (.id >> (==) source.id)
                        (\s -> { s | enabled = not s.enabled })
                        model.collection
            in
                (!)
                    { model | collection = newCollection }
                    [ updateEnabledSourceIds newCollection
                    , storeSources newCollection
                    ]



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveTags ProcessTagsStep ]
