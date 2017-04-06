module Sources.Processing
    exposing
        ( takeFirstStep
        , takeTreeStep
        , takeTagsStep
          --
        , findTagsContextSource
        , tracksFromTagsContext
        )

{-| Processing.

    ## How it works

    This describes the process for a single source.

    1. Get a file tree/list from the source
       -> This can happen in multiple steps as with Amazon S3.
          A command is issued for each step of this process.
    2. Get the tags (ie. metadata) for each file that we found.
       -> This also happens in multiple steps, so that we can flush
          every x tracks while processing.
          A command is issued for each step of this process.
-}

import Date exposing (Date)
import List.Extra as List
import Sources.Ports as Ports
import Sources.Types exposing (..)
import Tracks.Types exposing (..)
import Tracks.Utils exposing (makeTrack)
import Utils exposing (do)


-- Services

import Sources.Services.AmazonS3 as AmazonS3


-- Settings


{-| How much tags do we want to process
    before we send them back to Elm.

    eg. After we got the tags for 100 tracks,
    we store these and continue with the rest.
-}
tagsBatchSize : Int
tagsBatchSize =
    100



-- {public} Steps


takeFirstStep : Date -> Source -> Cmd Msg
takeFirstStep currentDate source =
    let
        initialContext =
            { filePaths = []
            , source = source
            , treeMarker = TheBeginning
            }
    in
        makeTree initialContext currentDate


takeTreeStep : ProcessingContext -> String -> CmdWithTimestamp
takeTreeStep context response currentDate =
    let
        newContext =
            handleTreeResponse context response
    in
        case newContext.treeMarker of
            TheBeginning ->
                Cmd.none

            InProgress _ ->
                makeTree newContext currentDate

            TheEnd ->
                newContext
                    |> processingContextToTagsContext
                    |> ProcessTagsStep
                    |> do


takeTagsStep : Date -> ProcessingContextForTags -> Source -> Maybe (Cmd Msg)
takeTagsStep currentDate tagsCtx source =
    let
        ( filesToProcess, nextFiles ) =
            List.splitAt tagsBatchSize tagsCtx.nextFilePaths

        newTagsCtx =
            { nextFilePaths = nextFiles
            , receivedFilePaths = filesToProcess
            , receivedTags = []
            , sourceId = source.id
            , urlsForTags = makeTrackUrls currentDate source filesToProcess
            }
    in
        filesToProcess
            |> List.head
            |> Maybe.map (always (Ports.requestTags newTagsCtx))



-- Tree


handleTreeResponse : ProcessingContext -> String -> ProcessingContext
handleTreeResponse context response =
    let
        parsingFunc =
            case context.source.service of
                AmazonS3 ->
                    AmazonS3.parseTreeResponse

        parsedResponse =
            parsingFunc response
    in
        { context
            | filePaths = context.filePaths ++ parsedResponse.filePaths
            , treeMarker = parsedResponse.marker
        }


makeTree : ProcessingContext -> CmdWithTimestamp
makeTree context =
    let
        fn =
            case context.source.service of
                AmazonS3 ->
                    AmazonS3.makeTree
    in
        fn context.source.data context.treeMarker (ProcessTreeStep context)



-- Tags


makeTrackUrls : Date -> Source -> List String -> List TagUrls
makeTrackUrls currentDate source filePaths =
    let
        maker =
            case source.service of
                AmazonS3 ->
                    AmazonS3.makeTrackUrl

        mapFn =
            \path ->
                { getUrl = maker currentDate source.data Get path
                , headUrl = maker currentDate source.data Head path
                }
    in
        List.map mapFn filePaths



-- {Public} Utils


findTagsContextSource : ProcessingContextForTags -> List Source -> Maybe Source
findTagsContextSource tagsContext =
    List.find (.id >> (==) tagsContext.sourceId)


tracksFromTagsContext : ProcessingContextForTags -> List Track
tracksFromTagsContext context =
    context.receivedTags
        |> List.zip context.receivedFilePaths
        |> List.map (makeTrack context.sourceId)
        |> Debug.log "tracks"



-- {Private} Utils


processingContextToTagsContext : ProcessingContext -> ProcessingContextForTags
processingContextToTagsContext context =
    { nextFilePaths = context.filePaths
    , receivedFilePaths = []
    , receivedTags = []
    , sourceId = context.source.id
    , urlsForTags = []
    }
