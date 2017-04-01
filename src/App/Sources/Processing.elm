module Sources.Processing
    exposing
        ( takeFirstStep
        , takeTreeStep
        , takeTagsStep
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
import List.Extra as ListEx
import Sources.Ports as Ports
import Sources.Types exposing (..)
import Task
import Tracks.Types exposing (..)


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


takeFirstStep : Source -> CmdWithTimestamp
takeFirstStep source =
    let
        initialContext =
            { filePaths = []
            , source = source
            , treeMarker = TheBeginning
            }
    in
        makeTree initialContext


takeTreeStep : ProcessingContext -> String -> ( ProcessingContext, Maybe CmdWithTimestamp )
takeTreeStep context response =
    let
        newContext =
            handleTreeResponse context response
    in
        case context.treeMarker of
            InProgress _ ->
                ( newContext
                , Just (makeTree context)
                )

            _ ->
                ( newContext
                , Nothing
                )


takeTagsStep : Date -> ProcessingContext -> Maybe (Cmd Msg)
takeTagsStep currentDate context =
    let
        ( filesToProcess, nextFiles ) =
            ListEx.splitAt tagsBatchSize context.filePaths

        tagsContext =
            { filePaths = nextFiles
            , receivedTags = []
            , urlsForTags = makeTrackUrls currentDate context filesToProcess
            }
    in
        if List.length filesToProcess > 0 then
            Just (Ports.requestTags tagsContext)
        else
            Nothing



-- Tree


handleTreeResponse : ProcessingContext -> String -> ProcessingContext
handleTreeResponse context response =
    case context.source of
        AmazonS3 _ ->
            AmazonS3.handleTreeResponse context response


makeTree : ProcessingContext -> CmdWithTimestamp
makeTree context =
    let
        msg =
            ProcessTreeStep context
    in
        case context.source of
            AmazonS3 sourceData ->
                AmazonS3.makeTree msg sourceData context.treeMarker



-- Tags


makeTrackUrls : Date -> ProcessingContext -> List String -> List TagUrls
makeTrackUrls currentDate context filePaths =
    case context.source of
        AmazonS3 sourceDate ->
            let
                maker =
                    AmazonS3.makeTrackUrl currentDate sourceDate

                mapFn =
                    \path ->
                        { getUrl = maker Get path
                        , headUrl = maker Head path
                        }
            in
                List.map mapFn filePaths
