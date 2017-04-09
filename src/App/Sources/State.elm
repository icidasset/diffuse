module Sources.State exposing (..)

import Date
import Dict
import Maybe.Ext as Maybe
import Maybe.Extra as Maybe
import Queue.Types
import Sources.Ports as Ports
import Sources.Processing as Processing
import Sources.Types exposing (..)
import Sources.Utils exposing (..)
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


initialCommands : Cmd Msg
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
        Process ->
            let
                isProcessing =
                    model.collection
                        |> List.head
                        |> Maybe.map (always model.collection)
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

           TODO: Remove old tracks
        -}
        ProcessTreeStep ctx (Ok resp) ->
            ($)
                model
                [ Processing.takeTreeStep ctx resp model.timestamp ]
                []

        ProcessTreeStep _ (Err err) ->
            (!)
                { model
                    | isProcessing = Nothing
                    , processingError = Just (toString err)
                }
                []

        {- Processing step,
           Phase 2, `makeTags`.
           ie. get the tags for each file in the file list.
        -}
        ProcessTagsStep tagsCtx ->
            let
                insert =
                    tagsCtx
                        |> Processing.tracksFromTagsContext
                        |> Queue.Types.AddTracks
                        |> TopLevel.QueueMsg
                        |> do

                cmd =
                    model.isProcessing
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
                        |> Queue.Types.RemoveTracks
                        |> TopLevel.QueueMsg
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
                    { model | collection = newCollection, processingError = Nothing }
                    [ do Process ]
                    [ storeSources newCollection ]



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveTags ProcessTagsStep ]
