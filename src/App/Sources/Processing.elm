module Sources.Processing
    exposing
        ( takeFirstStep
        , takeTreeStep
        , takeTagsStep
          --
        , decodeError
        , findTagsContextSource
        , makeTrackUrl
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
import Maybe.Extra as Maybe
import Regex
import Sources.Ports as Ports
import Sources.Types exposing (..)
import Tracks.Types exposing (TagUrls, Track, makeTrack)
import Utils exposing (do)


-- Services

import Sources.Services.AmazonS3 as AmazonS3


-- Settings


{-| How much tags do we want to process
    before we send them back to Elm.

    eg. After we got the tags for 50 tracks,
    we store these and continue with the rest.
-}
tagsBatchSize : Int
tagsBatchSize =
    50



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


takeTreeStep : ProcessingContext -> String -> List Track -> CmdWithTimestamp
takeTreeStep context response associatedTracks currentDate =
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
                let
                    filteredContext =
                        selectMusicFiles newContext

                    remove =
                        \list path ->
                            list
                                |> List.findIndex (\x -> x == path)
                                |> Maybe.map (\idx -> List.removeAt idx list)
                                |> Maybe.withDefault list

                    ( pathsLeft, pathsToRemove, _ ) =
                        List.foldr
                            (\track ( left, toRemove, srcOfTruth ) ->
                                let
                                    path =
                                        track.path
                                in
                                    if List.member path srcOfTruth then
                                        ( path :: left, toRemove, remove srcOfTruth path )
                                    else
                                        ( left, path :: toRemove, srcOfTruth )
                            )
                            ( [], [], filteredContext.filePaths )
                            associatedTracks
                in
                    Cmd.batch
                        [ filteredContext
                            |> selectNonExisting pathsLeft
                            |> processingContextToTagsContext
                            |> ProcessTagsStep
                            |> do
                        , pathsToRemove
                            |> ProcessTreeStepRemoveTracks context.source.id
                            |> do
                        ]


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


selectMusicFiles : ProcessingContext -> ProcessingContext
selectMusicFiles context =
    let
        regex =
            Regex.regex "\\.(mp3|mp4|m4a)$"
    in
        { context
            | filePaths = List.filter (Regex.contains regex) context.filePaths
        }


selectNonExisting : List String -> ProcessingContext -> ProcessingContext
selectNonExisting existingPaths context =
    let
        notMember =
            flip List.notMember
    in
        { context
            | filePaths = List.filter (notMember existingPaths) context.filePaths
        }



-- {Public} Utils


decodeError : Source -> String -> String
decodeError source =
    case source.service of
        AmazonS3 ->
            AmazonS3.parseErrorResponse


findTagsContextSource : ProcessingContextForTags -> List Source -> Maybe Source
findTagsContextSource tagsContext =
    List.find (.id >> (==) tagsContext.sourceId)


makeTrackUrl : Date -> Source -> String -> String
makeTrackUrl currentDate source filePath =
    case source.service of
        AmazonS3 ->
            AmazonS3.makeTrackUrl currentDate source.data Get filePath


tracksFromTagsContext : ProcessingContextForTags -> List Track
tracksFromTagsContext context =
    context.receivedTags
        |> List.zip context.receivedFilePaths
        |> List.filter (Tuple.second >> Maybe.isJust)
        |> List.map (Tuple.mapSecond (Maybe.withDefault Tracks.Types.emptyTags))
        |> List.map (makeTrack context.sourceId)



-- {Private} Utils


processingContextToTagsContext : ProcessingContext -> ProcessingContextForTags
processingContextToTagsContext context =
    { nextFilePaths = context.filePaths
    , receivedFilePaths = []
    , receivedTags = []
    , sourceId = context.source.id
    , urlsForTags = []
    }
