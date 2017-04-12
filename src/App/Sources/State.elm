module Sources.State exposing (..)

import Date
import Dict
import List.Extra as List
import Maybe.Ext as Maybe
import Maybe.Extra as Maybe
import Sources.Ports as Ports
import Sources.Processing as Processing
import Sources.Types exposing (..)
import Sources.Utils exposing (..)
import Tracks.Types exposing (emptyTrack)
import Types as TopLevel
import Utils exposing (do)


-- Services

import Sources.Services.AmazonS3 as AmazonS3


-- 💧


initialModel : TopLevel.ProgramFlags -> Model
initialModel flags =
    { collection = decodeSources flags
    , isProcessing = Nothing
    , newSource = makeSource AmazonS3 AmazonS3.initialData
    , processingError = Nothing
    , timestamp = Date.fromTime 0
    }


initialCommands : Cmd TopLevel.Msg
initialCommands =
    Cmd.none



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
            in
                ($)
                    { model | isProcessing = isProcessing }
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

        ProcessTreeStep _ (Err err) ->
            (!)
                { model
                    | isProcessing = Nothing
                    , processingError = Just (toString err)
                }
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
                    , storeSources newCollection
                    ]

        ------------------------------------
        -- Forms
        ------------------------------------
        SetNewSourceProperty source key value ->
            let
                newSource =
                    { source | data = Dict.insert key value source.data }
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
                        , newSource = makeSource AmazonS3 AmazonS3.initialData
                        , processingError = Nothing
                    }
                    []
                    [ do TopLevel.ProcessSources
                    , storeSources newCollection
                    ]



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveTags ProcessTagsStep ]
