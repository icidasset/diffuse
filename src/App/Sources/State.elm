module Sources.State exposing (..)

import Date
import List.Extra as ListEx
import Navigation
import Sources.Ports as Ports
import Sources.Processing as Processing
import Sources.Types exposing (..)
import Time
import Tracks.Types as Tracks
import Utils exposing (do)


-- Services

import Sources.Services.AmazonS3 as AmazonS3


-- 💧


initialModel : Model
initialModel =
    { isProcessing = Nothing
    , newSource = newSource (AmazonS3 AmazonS3.initialProperties)
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

           TODO: Get `Source` from model.isProcessing?
        -}
        ProcessTagsStep tagsContext ->
            case getTagsContextSource tagsContext model.sources of
                {- Ideally this should never happen, but just in case ... -}
                Nothing ->
                    (!)
                        { model
                            | processingError =
                                Just "Could not find source during ProcessTagsStep"
                        }
                        []

                {- Actual processing -}
                Just source ->
                    let
                        context =
                            tagsContextToProcessingContext tagsContext source

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
           Phase 3, store the tracks.
        -}
        ProcessInsertionStep source tagsContext ->
            (!)
                { model | tracks = tracksFromTagsContext tagsContext }
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



-- Tracks


tracksFromTagsContext : ProcessingContextForTags -> List Tracks.Track
tracksFromTagsContext context =
    context.receivedTags
        |> ListEx.zip context.receivedFilePaths
        |> List.map (makeTrack context.sourceId)
        |> Debug.log "tracks"


makeTrack : String -> ( String, Tracks.Tags ) -> Tracks.Track
makeTrack sourceId ( path, tags ) =
    { path = path
    , sourceId = sourceId
    , tags = tags
    }



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



-- Utils


getTagsContextSource : ProcessingContextForTags -> List Source -> Maybe Source
getTagsContextSource tagsContext =
    ListEx.find (.id >> (==) tagsContext.sourceId)


tagsContextToProcessingContext : ProcessingContextForTags -> Source -> ProcessingContext
tagsContextToProcessingContext context source =
    { filePaths = context.nextFilePaths
    , source = source
    , treeMarker = TheBeginning
    }


processingContextToTagsContext : ProcessingContext -> ProcessingContextForTags
processingContextToTagsContext context =
    { nextFilePaths = context.filePaths
    , receivedFilePaths = []
    , receivedTags = []
    , sourceId = context.source.id
    , urlsForTags = []
    }
