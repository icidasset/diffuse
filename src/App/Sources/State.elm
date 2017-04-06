module Sources.State exposing (..)

import Date
import List.Extra as List
import Maybe.Extensions as Maybe
import Maybe.Extra as Maybe
import Navigation
import Sources.Ports as Ports
import Sources.Processing as Processing
import Sources.Types exposing (..)
import Time
import Tracks.Types as Tracks exposing (makeTrack)
import Utils exposing (do)


-- Services

import Sources.Services.AmazonS3 as AmazonS3


-- 💧


initialModel : Model
initialModel =
    { isProcessing = Nothing
    , newSource = makeSource (AmazonS3 AmazonS3.initialProperties)
    , processingError = Nothing
    , sources = []
    , tracks = []
    , timestamp = Date.fromTime 0
    }


initialCommands : Cmd Msg
initialCommands =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd Msg )
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
                    model.sources
                        |> List.head
                        |> Maybe.map (always model.sources)
                        |> Maybe.preferFirst model.isProcessing

                command =
                    model.sources
                        |> List.head
                        |> Maybe.map (Processing.takeFirstStep model.timestamp)
                        |> Maybe.preferSecond (Maybe.map (always Cmd.none) model.isProcessing)
                        |> Maybe.withDefault Cmd.none
            in
                (,)
                    { model | isProcessing = isProcessing }
                    command

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
                (!)
                    { model | isProcessing = Maybe.map Tuple.first maybe }
                    [ maybe
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault Cmd.none
                    ]

        {- Processing step,
           Phase 1, `makeTree`.
           ie. make a file list/tree.
        -}
        ProcessTreeStep ctx (Ok resp) ->
            (!)
                model
                [ Processing.takeTreeStep ctx resp model.timestamp ]

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
                    do (ProcessInsertionStep tagsCtx)

                cmd =
                    model.isProcessing
                        |> Maybe.andThen (Processing.findTagsContextSource tagsCtx)
                        |> Maybe.andThen (Processing.takeTagsStep model.timestamp tagsCtx)
                        |> Maybe.withDefault (do ProcessNextInLine)
            in
                (!) model [ cmd, insert ]

        {- Processing step,
           Phase 3, store the tracks.
        -}
        ProcessInsertionStep tagsCtx ->
            (!)
                { model | tracks = Processing.tracksFromTagsContext tagsCtx }
                [ do SyncTracks ]

        ------------------------------------
        -- TODO : Firebase
        ------------------------------------
        SyncSources ->
            (!) model []

        SyncTracks ->
            (!) model []

        ------------------------------------
        -- Forms
        ------------------------------------
        SetNewSourceProperty source key value ->
            (!)
                { model
                    | newSource = setNewSourceProperty key value model.newSource
                }
                []

        SubmitNewSourceForm ->
            (!)
                { model
                    | processingError = Nothing
                    , sources = (setProperSourceId model model.newSource) :: model.sources
                }
                [ do Process
                , do SyncSources
                ]



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveTags ProcessTagsStep ]



-- Forms


setNewSourceProperty : String -> String -> Source -> Source
setNewSourceProperty key value source =
    let
        newSourceData =
            case source.data of
                AmazonS3 s3Data ->
                    value
                        |> AmazonS3.translateTo s3Data key
                        |> AmazonS3
    in
        { source | data = newSourceData }



-- Sources


setProperSourceId : Model -> Source -> Source
setProperSourceId model source =
    { source
        | id =
            model.timestamp
                |> Date.toTime
                |> Time.inMilliseconds
                |> round
                |> toString
                |> (flip String.append) (List.length model.sources |> (+) 1 |> toString)
    }
