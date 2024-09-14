module Brain.Sources.Processing.State exposing (..)

import Alien
import Brain.Common.State as Common
import Brain.Sources.Processing.Common exposing (..)
import Brain.Sources.Processing.Steps as Steps
import Brain.Sources.Processing.Types as Processing exposing (..)
import Brain.Tracks.State as Tracks
import Brain.Types exposing (..)
import Dict.Ext as Dict
import Http
import Json.Decode as Json
import Json.Encode as Encode
import List.Extra as List
import Return exposing (..)
import Sources exposing (Source)
import Sources.Processing exposing (..)
import Sources.Processing.Encoding as Processing



-- 📣


update : Processing.Msg -> Manager
update msg =
    case msg of
        Process a ->
            process a

        NextInLine ->
            nextInLine

        StopProcessing ->
            stopProcessing

        -----------------------------------------
        -- Steps
        -----------------------------------------
        PrepareStep a b ->
            prepareStep a b

        TreeStep a b ->
            treeStep a b

        TreeStepRemoveTracks a b ->
            treeStepRemoveTracks a b

        TagsStep a ->
            tagsStep a



-- 🔱


process : Json.Value -> Manager
process json =
    -- Only proceed to the processing if we got all the necessary data,
    -- otherwise report an error in the UI.
    case Json.decodeValue Processing.argumentsDecoder json of
        Ok arguments ->
            process_ arguments

        Err err ->
            Common.reportUI Alien.ProcessSources (Json.errorToString err)



{- If already processing, do nothing.
   If there are no sources, do nothing.
   If there are sources, start processing the first source.
-}


process_ : { origin : String, sources : List Source } -> Manager
process_ { origin, sources } model =
    let
        tracks =
            model.hypaethralUserData.tracks

        filter s =
            List.filter (.sourceId >> (==) s.id) tracks

        all =
            sources
                |> List.sortBy (.data >> Dict.fetch "name" "")
                |> List.map (\s -> ( s, filter s ))
    in
    case
        ( isProcessing model.processingStatus || List.isEmpty sources
        , List.uncons all
        )
    of
        ( False, Just ( ( s, t ), future ) ) ->
            return
                { model | origin = origin, processingStatus = Processing ( s, t ) future }
                (Steps.takeFirstStep origin model.currentTime s)

        _ ->
            Return.singleton model



{- If not processing, do nothing.
   If there are no sources left, do nothing.
   If there are sources left, start processing the next source in line.
-}


nextInLine : Manager
nextInLine model =
    case model.processingStatus of
        Processing ( processedSource, _ ) (( source, tracks ) :: rest) ->
            source
                |> Steps.takeFirstStep model.origin model.currentTime
                |> return
                    { model | processingStatus = Processing ( source, tracks ) rest }
                |> andThen
                    (processedSource.id
                        |> Encode.string
                        |> Common.giveUI Alien.FinishedProcessingSource
                    )

        _ ->
            Common.nudgeUI
                Alien.FinishedProcessingSources
                { model | processingStatus = NotProcessing }



{- STOP! -}


stopProcessing : Manager
stopProcessing model =
    Return.singleton { model | processingStatus = NotProcessing }



-- PHASE 1
----------
-- Prepare for processing.


prepareStep : Context -> Result Http.Error String -> Manager
prepareStep context result model =
    case result of
        Ok response ->
            model.currentTime
                |> Steps.takePrepareStep context response
                |> return model

        Err err ->
            model
                |> nextInLine
                |> andThen (reportHttpError context.source err)



-- PHASE 2
----------
-- Make a file list/tree.


treeStep : Context -> Result Http.Error String -> Manager
treeStep context result model =
    case result of
        Ok response ->
            case model.processingStatus of
                Processing ( _, tracks ) rest ->
                    return
                        { model | processingStatus = Processing ( context.source, tracks ) rest }
                        (Steps.takeTreeStep context response tracks model.currentTime)

                NotProcessing ->
                    Return.singleton model

        Err err ->
            model
                |> nextInLine
                |> andThen (reportHttpError context.source err)


treeStepRemoveTracks : String -> List String -> Manager
treeStepRemoveTracks sourceId filePaths model =
    let
        encodedData =
            Encode.object
                [ ( "filePaths", Encode.list Encode.string filePaths )
                , ( "sourceId", Encode.string sourceId )
                ]
    in
    model
        |> Common.giveUI Alien.RemoveTracksByPath encodedData
        |> andThen (Tracks.removeByPaths { sourceId = sourceId, paths = filePaths })



-- PHASE 3
----------
-- Get the tags for each file in the file list.


tagsStep : ContextForTags -> Manager
tagsStep tagsContext model =
    let
        maybeCmd =
            case model.processingStatus of
                Processing ( source, _ ) _ ->
                    Steps.takeTagsStep model.currentTime tagsContext source

                NotProcessing ->
                    Just Cmd.none

        tracksToAdd =
            tagsContext
                |> tracksFromTagsContext
                |> List.map (\track -> { track | insertedAt = model.currentTime })

        amountLeft =
            List.length tagsContext.nextFilePaths

        progressPercentage =
            0.05 + 0.95 * (1 - toFloat amountLeft / toFloat tagsContext.amount)

        progress =
            [ ( "progress", Encode.float progressPercentage )
            , ( "sourceId", Encode.string tagsContext.sourceId )
            ]
    in
    maybeCmd
        |> Maybe.map (return model)
        |> Maybe.withDefault (nextInLine model)
        |> andThen (Tracks.add tracksToAdd)
        |> andThen (Common.giveUI Alien.ReportProcessingProgress <| Encode.object progress)
