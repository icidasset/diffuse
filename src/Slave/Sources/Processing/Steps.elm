module Sources.Processing.Steps
    exposing
        ( takeFirstStep
        , takePrepareStep
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
import Http
import List.Extra as List
import Maybe.Extra as Maybe
import Response.Ext exposing (do)
import Set
import Slave.Events
import Slave.Types exposing (AlienMsg(UpdateSourceData))
import Sources.Encoding
import Sources.Services as Services
import Sources.Processing.Ports as Ports
import Sources.Processing.Types exposing (..)
import Sources.Types exposing (Source)
import Tracks.Types exposing (TagUrls, Track, makeTrack)


-- Settings


{-| How much tags do we want to process
before we send them back to Elm.

    eg. After we got the tags for 50 tracks,
    we store these and continue with the rest.

-}
tagsBatchSize : Int
tagsBatchSize =
    50



-- {public} 1st step


takeFirstStep : String -> Date -> Source -> Cmd Msg
takeFirstStep origin currentDate source =
    let
        initialContext =
            { filePaths = []
            , origin = origin
            , preparationMarker = TheBeginning
            , source = source
            , treeMarker = TheBeginning
            }
    in
        prepare initialContext currentDate



-- {public} 2nd step


takePrepareStep : Context -> String -> Date -> ( Cmd Msg, Cmd Slave.Types.Msg )
takePrepareStep context response currentDate =
    context
        |> handlePreparationResponse response
        |> intoPreparationCommands currentDate



-- {public} 3rd step


takeTreeStep : Context -> String -> List Track -> Date -> Cmd Msg
takeTreeStep context response associatedTracks currentDate =
    context
        |> handleTreeResponse response
        |> intoTreeCommand associatedTracks currentDate



-- {public} 4th step


takeTagsStep : Date -> ContextForTags -> Source -> Maybe (Cmd Msg)
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
            |> Maybe.map (always (getTags newTagsCtx))



-- Preparation


handlePreparationResponse : String -> Context -> Context
handlePreparationResponse response context =
    let
        answer =
            Services.parsePreparationResponse
                context.source.service
                response
                context.source.data
                context.preparationMarker

        source =
            context.source
    in
        { context
            | preparationMarker = answer.marker
            , source = { source | data = answer.sourceData }
        }


intoPreparationCommands : Date -> Context -> ( Cmd Msg, Cmd Slave.Types.Msg )
intoPreparationCommands currentDate context =
    case context.preparationMarker of
        TheBeginning ->
            ( Cmd.none
            , Cmd.none
            )

        -- Still preparing,
        -- carry on.
        InProgress _ ->
            ( prepare context currentDate
            , Cmd.none
            )

        -- The preparation is completed,
        -- continue to the next step.
        TheEnd ->
            let
                updatedSource =
                    context.source

                data =
                    Sources.Encoding.encode updatedSource
            in
                ( -- Make a file tree, the next step.
                  -- 🚀
                  makeTree context currentDate
                  -- Update source data in main `App`.
                , Slave.Events.issueWithData UpdateSourceData data
                )


prepare : Context -> Date -> Cmd Msg
prepare context currentDate =
    let
        maybePreparationRequest =
            Services.prepare
                context.source.service
                context.origin
                context.source.data
                context.preparationMarker
    in
        case maybePreparationRequest of
            Just request ->
                Http.send (PrepareStep context) request

            Nothing ->
                -- Some services don't need to prepare for processing.
                -- 🚀
                makeTree context currentDate



-- Tree


handleTreeResponse : String -> Context -> Context
handleTreeResponse response context =
    let
        parsingFunc =
            Services.parseTreeResponse context.source.service

        parsedResponse =
            parsingFunc response context.treeMarker
    in
        { context
            | filePaths = context.filePaths ++ parsedResponse.filePaths
            , treeMarker = parsedResponse.marker
        }


intoTreeCommand : List Track -> Date -> Context -> Cmd Msg
intoTreeCommand associatedTracks currentDate context =
    case context.treeMarker of
        TheBeginning ->
            Cmd.none

        -- Still building the tree,
        -- carry on.
        --
        InProgress _ ->
            makeTree context currentDate

        -- The tree's been build,
        -- continue to the next step.
        --
        TheEnd ->
            let
                filteredFiles =
                    Services.postProcessTree context.source.service context.filePaths

                postContext =
                    { context | filePaths = filteredFiles }

                pathsSourceOfTruth =
                    postContext.filePaths

                pathsCurrent =
                    List.map .path associatedTracks

                ( pathsAdded, pathsRemoved ) =
                    separate pathsCurrent pathsSourceOfTruth
            in
                Cmd.batch
                    [ -- Get tags from tracks, the next step.
                      -- 🚀
                      postContext
                        |> (\ctx -> { ctx | filePaths = pathsAdded })
                        |> contextToTagsContext
                        |> TagsStep
                        |> do

                    -- Remove tracks
                    , if not (List.isEmpty pathsRemoved) then
                        pathsRemoved
                            |> TreeStepRemoveTracks context.source.id
                            |> do
                      else
                        Cmd.none
                    ]


makeTree : Context -> Date -> Cmd Msg
makeTree context currentDate =
    currentDate
        |> Services.makeTree context.source.service context.source.data context.treeMarker
        |> Http.send (TreeStep context)


separate : List String -> List String -> ( List String, List String )
separate current srcOfTruth =
    let
        setCurrent =
            Set.fromList current

        setSrcOfTruth =
            Set.fromList srcOfTruth
    in
        (,)
            -- Added
            (Set.diff setSrcOfTruth setCurrent |> Set.toList)
            -- Removed
            (Set.diff setCurrent setSrcOfTruth |> Set.toList)



-- Tags


getTags : ContextForTags -> Cmd Msg
getTags context =
    Ports.requestTags context


makeTrackUrls : Date -> Source -> List String -> List TagUrls
makeTrackUrls currentDate source filePaths =
    let
        maker =
            Services.makeTrackUrl source.service

        mapFn =
            \path ->
                { getUrl = maker currentDate source.data Get path
                , headUrl = maker currentDate source.data Head path
                }
    in
        List.map mapFn filePaths



-- {public} Utils


findTagsContextSource : ContextForTags -> List Source -> Maybe Source
findTagsContextSource tagsContext =
    List.find (.id >> (==) tagsContext.sourceId)


tracksFromTagsContext : ContextForTags -> List Track
tracksFromTagsContext context =
    context.receivedTags
        |> List.zip context.receivedFilePaths
        |> List.filter (Tuple.second >> Maybe.isJust)
        |> List.map (Tuple.mapSecond (Maybe.withDefault Tracks.Types.emptyTags))
        |> List.map (makeTrack context.sourceId)



-- Utils


contextToTagsContext : Context -> ContextForTags
contextToTagsContext context =
    { nextFilePaths = context.filePaths
    , receivedFilePaths = []
    , receivedTags = []
    , sourceId = context.source.id
    , urlsForTags = []
    }
