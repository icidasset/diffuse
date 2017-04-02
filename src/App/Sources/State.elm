module Sources.State exposing (..)

import Date
import Navigation
import Sources.Ports as Ports
import Sources.Processing as Processing
import Sources.Types exposing (..)
import Utils exposing (do)


-- Services

import Sources.Services.AmazonS3 as AmazonS3


-- 💧


initialModel : Model
initialModel =
    { isProcessing = Nothing
    , newSource = AmazonS3 AmazonS3.initialProperties
    , processingError = Nothing
    , sources = []
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
        {- If already processing, keep doing as you were.
           If there are no sources, do nothing.
           If there are sources, start processing the first source.
        -}
        Process ->
            let
                isProcessing =
                    case model.isProcessing of
                        Just _ ->
                            model.isProcessing

                        Nothing ->
                            if List.length model.sources > 0 then
                                Just model.sources
                            else
                                Nothing

                command =
                    case model.isProcessing of
                        Just _ ->
                            Cmd.none

                        Nothing ->
                            case List.head model.sources of
                                Just source ->
                                    {- 🚀 STEP -}
                                    Processing.takeFirstStep source model.timestamp

                                Nothing ->
                                    Cmd.none
            in
                (,)
                    { model | isProcessing = isProcessing }
                    command

        {- If not processing, do nothing.
           If there are no sources left, set `isProcessing` to `Nothing`.
           If there are sources left, start processing the next source in line.
        -}
        ProcessNextInLine ->
            case model.isProcessing of
                Just sources ->
                    let
                        newSources =
                            List.drop 1 sources
                    in
                        case List.head newSources of
                            {- 🚀 STEP -}
                            Just source ->
                                (!)
                                    { model | isProcessing = Just newSources }
                                    [ Processing.takeFirstStep source model.timestamp ]

                            Nothing ->
                                (,)
                                    { model | isProcessing = Nothing }
                                    Cmd.none

                Nothing ->
                    ( model, Cmd.none )

        {- Processing step,
           Phase 1, `makeTree`.
           ie. make a file list/tree.
        -}
        ProcessTreeStep context (Ok stringResponse) ->
            let
                ( newContext, maybeCommand ) =
                    Processing.takeTreeStep context stringResponse
            in
                (!)
                    model
                    [ case maybeCommand of
                        Just getCmd ->
                            getCmd model.timestamp

                        Nothing ->
                            {- 🚀 STEP -}
                            newContext
                                |> processingContextToTagsContext
                                |> ProcessTagsStep
                                |> do
                    ]

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

           TODO: Could be improved:
           - Remove `Debug.crash` calls
           - Retrieve related `source` in a better way
        -}
        ProcessTagsStep tagsContext ->
            let
                ( source, context ) =
                    case model.isProcessing of
                        Just sources ->
                            case List.head sources of
                                Just source ->
                                    ( source
                                    , tagsContextToProcessingContext source tagsContext
                                    )

                                Nothing ->
                                    Debug.crash "Invalid state occurred, fix it."

                        Nothing ->
                            Debug.crash "Invalid state occurred, fix it."

                insert =
                    tagsContext
                        |> ProcessInsertionStep source
                        |> do
            in
                case Processing.takeTagsStep model.timestamp context of
                    Just cmd ->
                        {- 🚀 STEP -}
                        (!) model [ cmd, insert ]

                    Nothing ->
                        {- 🍪 NEXT -}
                        (!) model [ do ProcessNextInLine, insert ]

        {- Processing step,
           Phase 3, store the data.
        -}
        ProcessInsertionStep source tagsContext ->
            let
                a =
                    Debug.log "tags" tagsContext.receivedTags
            in
                (!) model []

        ------------------------------------
        -- Forms
        ------------------------------------
        SetNewSource source ->
            (!)
                { model | newSource = source }
                []

        SetNewSourceProperty source propKey propValue ->
            let
                updatedSource =
                    case source of
                        AmazonS3 sourceData ->
                            propValue
                                |> AmazonS3.translateTo sourceData propKey
                                |> AmazonS3
            in
                (!) { model | newSource = updatedSource } []

        SubmitNewSourceForm ->
            (!)
                { model
                    | processingError = Nothing
                    , sources = model.newSource :: model.sources
                }
                [ do Process ]



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveTags ProcessTagsStep ]



-- Utils


tagsContextToProcessingContext : Source -> ProcessingContextForTags -> ProcessingContext
tagsContextToProcessingContext source context =
    { filePaths = context.filePaths
    , source = source
    , treeMarker = TheBeginning
    }


processingContextToTagsContext : ProcessingContext -> ProcessingContextForTags
processingContextToTagsContext context =
    { filePaths = context.filePaths
    , receivedTags = []
    , urlsForTags = []
    }
