module Brain.Sources.Processing.Steps exposing
    ( takeFirstStep
    , takePrepareStep
    , takeTagsStep
    , takeTreeStep
    )

{-| Processing.

    ## How it works

    This describes the process for a single source.

    1. Prepare the source for processing.
       -> For example, a source could get an access token first.
    2. Get a file tree/list from the source
       -> This can happen in multiple steps as with Amazon S3.
          A command is issued for each step of this process.
    3. Get the tags (ie. metadata) for each file that we found.
       -> This also happens in multiple steps, so that we can flush
          every x tracks while processing.
          A command is issued for each step of this process.

-}

import Alien
import Brain.Ports as Ports
import Brain.Sources.Processing.Common exposing (..)
import Brain.Sources.Processing.Types exposing (..)
import Brain.Types as Brain exposing (..)
import List.Extra as List
import Set
import Sources exposing (Source)
import Sources.Encoding
import Sources.Processing exposing (..)
import Sources.Services as Services
import Task.Extra exposing (do)
import Time
import Tracks exposing (Track)



-- SETTINGS


{-| How much tags do we want to process
before we send them back to Elm.

    eg. After we got the tags for 20 tracks,
    we store these and continue with the rest.

-}
tagsBatchSize : Int
tagsBatchSize =
    20



-- 1st STEP


takeFirstStep : String -> Time.Posix -> Source -> Cmd Brain.Msg
takeFirstStep origin currentTime source =
    let
        initialContext =
            { filePaths = []
            , origin = origin
            , preparationMarker = TheBeginning
            , source = source
            , treeMarker = TheBeginning
            }
    in
    prepare initialContext currentTime



-- 2nd STEP


takePrepareStep : Context -> String -> Time.Posix -> Cmd Brain.Msg
takePrepareStep context response currentTime =
    context
        |> handlePreparationResponse response currentTime
        |> intoPreparationCommands currentTime



-- 3rd STEP


takeTreeStep : Context -> String -> List Track -> Time.Posix -> Cmd Brain.Msg
takeTreeStep context response associatedTracks currentTime =
    context
        |> handleTreeResponse response
        |> intoTreeCommand associatedTracks currentTime



-- 4th STEP


takeTagsStep : Time.Posix -> ContextForTags -> Source -> Maybe (Cmd Brain.Msg)
takeTagsStep currentTime tagsCtx source =
    let
        ( filesToProcess, nextFiles ) =
            List.splitAt tagsBatchSize tagsCtx.nextFilePaths

        newTagsCtx =
            { amount = tagsCtx.amount
            , nextFilePaths = nextFiles
            , receivedFilePaths = filesToProcess
            , receivedTags = []
            , sourceId = source.id
            , urlsForTags = makeTrackUrls currentTime source filesToProcess
            }
    in
    if List.isEmpty filesToProcess then
        Nothing

    else
        Just (getTags newTagsCtx)



-----------------------------------------
-- ㊙️
-----------------------------------------
-- PREPARE


prepare : Context -> Time.Posix -> Cmd Brain.Msg
prepare context currentTime =
    let
        maybePreparationCommand =
            Services.prepare
                context.source.service
                context.origin
                context.source.data
                context.preparationMarker
                (ProcessingMsg << PrepareStep context)
    in
    case maybePreparationCommand of
        Just cmd ->
            cmd

        Nothing ->
            -- Some services don't need to prepare for processing.
            -- 🚀
            makeTree context currentTime


handlePreparationResponse : String -> Time.Posix -> Context -> Context
handlePreparationResponse response currentTime context =
    let
        answer =
            Services.parsePreparationResponse
                context.source.service
                response
                currentTime
                context.source.data
                context.preparationMarker

        source =
            context.source
    in
    { context
        | preparationMarker = answer.marker
        , source = { source | data = answer.sourceData }
    }


intoPreparationCommands : Time.Posix -> Context -> Cmd Brain.Msg
intoPreparationCommands currentTime context =
    case context.preparationMarker of
        TheBeginning ->
            Cmd.none

        -- Still preparing,
        -- carry on.
        --
        InProgress _ ->
            prepare context currentTime

        -- The preparation is completed,
        -- continue to the next step.
        --
        TheEnd ->
            let
                updatedSource =
                    context.source
            in
            Cmd.batch
                [ -- Make a file tree, the next step.
                  -- 🚀
                  makeTree context currentTime

                -- Update source data.
                , updatedSource
                    |> Sources.Encoding.encode
                    |> Alien.broadcast Alien.UpdateSourceData
                    |> Ports.toUI
                ]



-- TREE


makeTree : Context -> Time.Posix -> Cmd Brain.Msg
makeTree context currentTime =
    Services.makeTree
        context.source.service
        context.source.data
        context.treeMarker
        currentTime
        (ProcessingMsg << TreeStep context)


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


intoTreeCommand : List Track -> Time.Posix -> Context -> Cmd Brain.Msg
intoTreeCommand associatedTracks currentTime context =
    case context.treeMarker of
        TheBeginning ->
            Cmd.none

        -- Still building the tree,
        -- carry on.
        --
        InProgress _ ->
            makeTree context currentTime

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
                    |> ProcessingMsg
                    |> do

                -- Remove tracks
                , if not (List.isEmpty pathsRemoved) then
                    pathsRemoved
                        |> TreeStepRemoveTracks context.source.id
                        |> ProcessingMsg
                        |> do

                  else
                    Cmd.none
                ]


separate : List String -> List String -> ( List String, List String )
separate current srcOfTruth =
    let
        setCurrent =
            Set.fromList current

        setSrcOfTruth =
            Set.fromList srcOfTruth
    in
    ( -- Added
      --------
      Set.diff setSrcOfTruth setCurrent |> Set.toList
    , -- Removed
      ----------
      Set.diff setCurrent setSrcOfTruth |> Set.toList
    )



-- TAGS


getTags : ContextForTags -> Cmd Brain.Msg
getTags =
    Ports.requestTags


makeTrackUrls : Time.Posix -> Source -> List String -> List TagUrls
makeTrackUrls currentTime source filePaths =
    let
        maker =
            Services.makeTrackUrl source.service

        mapFn =
            \path ->
                { getUrl = maker currentTime source.id source.data Get path
                , headUrl = maker currentTime source.id source.data Head path
                }
    in
    List.map mapFn filePaths
